open! Base
open Acorn
module Cx = C.Syntax

let prelude =
  {|
#include <cstdio>
#include <algorithm>
#include <iostream>

static void HandleError(cudaError_t err, const char *file, int line) {
  if (err != cudaSuccess) {
    printf("%s in %s at line %d\12", cudaGetErrorString(err),
            file, line);
    exit(EXIT_FAILURE);
  }
}
#define HANDLE_ERROR(err) (HandleError( err, __FILE__, __LINE__ ))

template<typename T>
T *mallocHost(int64_t count) {
  return (T *) malloc(count * sizeof(T));
};

template<typename T>
__host__ T *mallocDevice(int64_t count) {
  T* array;
  HANDLE_ERROR(cudaMalloc((void **) &array, count * sizeof(T)));
  return array;
};

template<typename T>
__device__ T *mallocDeviceOnDevice(int64_t count) {
  return (T *) malloc(count * sizeof(T));
};

template<typename T>
T* copyHostToDeviceMalloc(T* hostArray, int64_t count) {
  T* deviceArray = mallocDevice<T>(count);
  HANDLE_ERROR(cudaMemcpy(deviceArray, hostArray, count * sizeof(T), cudaMemcpyHostToDevice));
  return deviceArray;
};

template<typename T>
T* copyDeviceToHostMalloc(T* deviceArray, int64_t count) {
  T* hostArray = mallocHost<T>(count);
  HANDLE_ERROR(cudaMemcpy(hostArray, deviceArray, count * sizeof(T), cudaMemcpyDeviceToHost));
  return hostArray;
};

template<typename T>
T* copyHostToDevice(T* deviceArray, T* hostArray, int64_t count) {
  HANDLE_ERROR(cudaMemcpy(deviceArray, hostArray, count * sizeof(T), cudaMemcpyHostToDevice));
  return deviceArray;
};

template<typename T>
T* copyDeviceToHost(T* hostArray, T* deviceArray, int64_t count) {
  HANDLE_ERROR(cudaMemcpy(hostArray, deviceArray, count * sizeof(T), cudaMemcpyDeviceToHost));
  return hostArray;
};

template<typename T>
void copyHostToHost(T* dest, T* source, int64_t count) {
  memcpy(dest, source, count * sizeof(T));
};

template<typename T>
void copyDeviceToDevice(T* dest, T* source, int64_t count) {
  HANDLE_ERROR(cudaMemcpy(dest, source, count * sizeof(T), cudaMemcpyDeviceToDevice));
};

template<typename T>
__host__ __device__ void copySameLoc(T* dest, T* source, int64_t count) {
  for (int64_t i = 0; i < count; i++) {
    dest[i] = source[i];
  }
};

struct Slice {
  int64_t* dims;
  int64_t dimCount;
};

int64_t sum(int64_t* arr, int64_t count) {
  int64_t s = 0;
  for(int64_t i = 0; i < count; i++) {
    s += arr[i];
  }
  return s;
};

__host__ __device__ int64_t sum(Slice slice) {
  int64_t s = 0;
  for(int64_t i = 0; i < slice.dimCount; i++) {
    s += slice.dims[i];
  }
  return s;
};

__host__ __device__ int64_t product(Slice slice) {
  int64_t p = 1;
  for(int64_t i = 0; i < slice.dimCount; i++) {
    p *= slice.dims[i];
  }
  return p;
};

template<typename T>
T at(T* elements, std::vector<int64_t> shape, std::vector<int64_t> index) {
  int64_t elementIndex = 0;
  int64_t stride = 1;
  for (auto i = index.size() - 1;; i--) {
    elementIndex += stride * index[i];
    stride *= shape[i];

    if (i == 0) break;
  }
  return elements[elementIndex];
};

template<typename T>
void printArray(T* elements, int64_t* dims, int64_t dimCount) {
  if (dimCount == 0) {
    std::cout << elements[0] << "\12";
    return;
  }

  std::vector<int64_t> indexes(dimCount, 0);

  while (true) {
    for (auto i = (int64_t)indexes.size() - 1; i >= 0; i--) {
      if (indexes[i] == 0) std::cout << "[";
      else break;
    }

    std::cout << at(elements, std::vector<int64_t>(dims, dims + dimCount), indexes);

    long i = (long)indexes.size() - 1;
    while (true) {
      if (i < 0) {
        std::cout << "\12";
        return;
      }

      indexes[i]++;
      if (indexes[i] == dims[i]) {
        std::cout << "]";
        indexes[i] = 0;
        i--;
      } else {
        if (i == indexes.size() - 1) std::cout << " ";
        else {
          std::cout << "\12";
          for (long j = 0; j <= i; j++) {
            std::cout << " ";
          }
        }
        break;
      }
    }
  }
};
|}
  |> String.split_lines
;;

module GenState = struct
  module CBuilder2 = struct
    type ('a, 'e) t = ('a, 'e) CBuilder.u

    include Monad.Make2 (struct
        type nonrec ('a, 'e) t = ('a, 'e) t

        let bind = CBuilder.bind
        let map = `Define_using_bind
        let return = CBuilder.return
      end)
  end

  include StateT.Make2 (CBuilder2)

  (* A definition sitting between a Type.t and a C type *)
  module CachedType = struct
    module T = struct
      type t =
        | Literal of Type.literal
        | Ptr of t
        | Box of
            { parameters : Type.sigmaParam list
            ; body : t
            }
        | Tuple of t list
      [@@deriving sexp_of, compare]
    end

    include T
    include Comparator.Make (T)

    let rec of_type ?(wrapInPtr = false) = function
      | Type.Atom (Literal literal) ->
        if wrapInPtr then Ptr (Literal literal) else Literal literal
      | Type.Array array ->
        let elementType = of_type ~wrapInPtr:false (Atom array.element) in
        Ptr elementType
      | Type.Atom (Sigma { parameters; body }) ->
        let box = Box { parameters; body = of_type ~wrapInPtr:false body } in
        if wrapInPtr then Ptr box else box
      | Type.Tuple elements -> Tuple (List.map elements ~f:(of_type ~wrapInPtr))
    ;;
  end

  type state =
    { statementsRev : C.statement list
    ; typeCache : C.type' Map.M(CachedType).t
    }

  let emptyState = { statementsRev = []; typeCache = Map.empty (module CachedType) }

  type ('a, 'e) u = (state, 'a, 'e) t

  let defineFun (name : CBuilder.name) ~(f : C.name -> (C.fun', 'e) u) : (C.name, 'e) u =
    makeF ~f:(fun inState ->
      CBuilder.defineFunL name ~f:(fun name -> run (f name) inState))
  ;;

  let defineStruct (name : CBuilder.name) ~(f : C.name -> (C.struct', 'e) u)
    : (C.name, 'e) u
    =
    makeF ~f:(fun inState ->
      CBuilder.defineStructL name ~f:(fun name -> run (f name) inState))
  ;;

  let write statement =
    let open Let_syntax in
    let%bind state = get () in
    let%map () = set { state with statementsRev = statement :: state.statementsRev } in
    ()
  ;;

  let block prog =
    let open Let_syntax in
    let%bind outerBlockState = get () in
    let%bind () = set { outerBlockState with statementsRev = [] } in
    let%bind () = prog in
    let%bind innerBlockState = get () in
    let%bind () =
      set { innerBlockState with statementsRev = outerBlockState.statementsRev }
    in
    return @@ List.rev innerBlockState.statementsRev
  ;;

  let createName name = returnF (CBuilder.createName name)
  let createId str = returnF (CBuilder.createId str)

  let createVar name makeDefine =
    let open Let_syntax in
    let%bind var = createName @@ NameOfStr { str = name; needsUniquifying = true } in
    let%bind () = write @@ makeDefine var in
    return @@ C.VarRef var
  ;;

  let createVarAuto name value =
    createVar name @@ fun name -> C.Define { name; type' = None; value = Some value }
  ;;

  let storeExpr ?(name = "storedExpr") (expr : C.expr) =
    let open Let_syntax in
    match expr with
    | Literal _ | VarRef _ -> (* Don't need to store if its a constant *) return expr
    | _ -> createVarAuto name expr
  ;;

  let writeIte ~cond ~thenBranch ~elseBranch =
    let open Let_syntax in
    let%bind thenBranch = block thenBranch in
    let%bind elseBranch = block elseBranch in
    write @@ C.Ite { cond; thenBranch; elseBranch }
  ;;

  let writeForLoop ~loopVar ~loopVarType ~initialValue ~cond ~loopVarUpdate ~body =
    let open Let_syntax in
    let%bind loopVar =
      createName @@ NameOfStr { str = loopVar; needsUniquifying = true }
    in
    let cond = cond @@ C.VarRef loopVar in
    let%bind body = block @@ body @@ C.VarRef loopVar in
    write @@ C.ForLoop { loopVar; loopVarType; initialValue; cond; loopVarUpdate; body }
  ;;

  let scope name type' value =
    let open Let_syntax in
    let%bind result =
      createVar name @@ fun name -> C.Define { name; type' = Some type'; value = None }
    in
    let%bind block =
      block
      @@
      let%bind value = value in
      write @@ Cx.(result := value)
    in
    let%bind () = write @@ C.Block block in
    return result
  ;;

  let comment comment = write @@ C.Comment comment
end

let tupleFieldName i = C.Name.StrName [%string "_%{i#Int}"]
let boxValueFieldName = C.Name.StrName "value"
let sliceDimCountFieldName = C.Name.StrName "dimCount"
let sliceDimsFieldName = C.Name.StrName "dims"
let blockIndex = Cx.(refStr "blockIdx" %. StrName "x")
let threadIndex = Cx.(refStr "threadIdx" %. StrName "x")

let genTypeForSort sort =
  match sort with
  | Sort.Dim -> C.Int64
  | Sort.Shape -> C.TypeRef (StrName "Slice")
;;

let genType ?(wrapInPtr = false) type' : (C.type', _) GenState.u =
  let open GenState.Let_syntax in
  let rec genType (type' : GenState.CachedType.t) =
    let%bind (state : GenState.state) = GenState.get () in
    let%bind cType =
      match Map.find state.typeCache type' with
      | Some cType -> return cType
      | None ->
        (match type' with
         | Literal IntLiteral -> return C.Int64
         | Literal FloatLiteral -> return C.Float64
         | Literal CharacterLiteral -> return C.Char
         | Literal BooleanLiteral -> return C.Bool
         | Ptr elementType ->
           let%bind elementType = genType elementType in
           return @@ C.Ptr elementType
         | Box { parameters; body } ->
           let%bind name =
             GenState.defineStruct
               (NameOfStr { str = "Box"; needsUniquifying = true })
               ~f:(fun _ ->
                 let paramFields =
                   List.map parameters ~f:(fun { binding; bound } ->
                     C.{ name = UniqueName binding; type' = genTypeForSort bound })
                 in
                 let%map valueType = genType body in
                 let valueField = C.{ name = boxValueFieldName; type' = valueType } in
                 valueField :: paramFields)
           in
           return @@ C.TypeRef name
         | Tuple elements ->
           let%bind elementTypes = elements |> List.map ~f:genType |> GenState.all in
           let%map name =
             GenState.defineStruct
               (NameOfStr { str = "Tuple"; needsUniquifying = true })
               ~f:(fun _ ->
                 return
                 @@ List.mapi elementTypes ~f:(fun i type' ->
                   C.{ name = tupleFieldName i; type' }))
           in
           C.TypeRef name)
    in
    let%bind () =
      GenState.modify ~f:(fun (state : GenState.state) ->
        { state with typeCache = Map.set state.typeCache ~key:type' ~data:cType })
    in
    return cType
  in
  genType @@ GenState.CachedType.of_type ~wrapInPtr type'
;;

type 'l hostOrDevice =
  | Host : host hostOrDevice
  | Device : device hostOrDevice

let genDim ({ const; refs; lens } : Index.dimension) =
  let addRefs init =
    Map.fold refs ~init ~f:(fun ~key:dimVar ~data:multiplier cValSoFar ->
      Cx.(cValSoFar + (refId dimVar * intLit multiplier)))
  in
  let addLens init =
    Map.fold lens ~init ~f:(fun ~key:sliceVar ~data:multiplier cValSoFar ->
      Cx.(cValSoFar + (refId sliceVar %. sliceDimCountFieldName * intLit multiplier)))
  in
  addLens @@ addRefs @@ Cx.(intLit const)
;;

let genShapeElementSize = function
  | Index.Add dim -> genDim dim
  | Index.ShapeRef shapeRef -> Cx.(callBuiltin "product" [ refId shapeRef ])
;;

let genShapeSize shape =
  List.fold shape ~init:Cx.(intLit 1) ~f:(fun acc se -> Cx.(acc * genShapeElementSize se))
;;

let genShapeDimCount shape =
  let knownDimCount, unknownDimCounts =
    List.fold shape ~init:(0, []) ~f:(fun (knownSoFar, unknownSoFar) shapeElement ->
      match shapeElement with
      | Index.Add _ -> knownSoFar + 1, unknownSoFar
      | Index.ShapeRef ref ->
        let refCount = Cx.(refId ref %. sliceDimCountFieldName) in
        knownSoFar, refCount :: unknownSoFar)
  in
  List.fold unknownDimCounts ~init:Cx.(intLit knownDimCount) ~f:Cx.( + )
;;

let reifyShapeIndexToMem ~mem shape =
  let open GenState.Let_syntax in
  let sumOffsets intOffset sliceSizeOffsets =
    List.fold
      sliceSizeOffsets
      ~init:Cx.(intLit intOffset)
      ~f:(fun offsetSoFar sliceSize ->
        Binop { op = "+"; arg1 = offsetSoFar; arg2 = sliceSize })
  in
  let%map intOffset, sliceSizeOffsets =
    List.fold
      shape
      ~init:(return (0, []))
      ~f:(fun offset shapeElement ->
        let%bind intOffset, sliceSizeOffsets = offset in
        let offset = sumOffsets intOffset sliceSizeOffsets in
        match shapeElement with
        | Index.Add dim ->
          let%map () = GenState.write Cx.(arrayDeref mem [ offset ] := genDim dim) in
          intOffset + 1, sliceSizeOffsets
        | Index.ShapeRef id ->
          let slice = Cx.refId id in
          let sliceSize = Cx.(slice %. sliceDimCountFieldName) in
          let%map () =
            GenState.write
            @@ C.Eval
                 Cx.(
                   callBuiltin
                     "copySameLoc"
                     ~typeArgs:[ Int64 ]
                     [ mem + offset; slice %. sliceDimCountFieldName; sliceSize ])
          in
          intOffset, sliceSize :: sliceSizeOffsets)
  in
  sumOffsets intOffset sliceSizeOffsets
;;

(** Chop the head element of the array shape off *)
let rec guillotineType = function
  | Type.Array { element; shape = _ :: [] } -> Type.Atom element
  | Type.Array { element; shape = _ :: head :: rest } ->
    Type.Array { element; shape = head :: rest }
  | Type.Tuple elements -> Type.Tuple (List.map elements ~f:guillotineType)
  | Type.Atom atom -> Type.Atom atom
;;

(* The result is a function that given an index returns the derefed array.
   It is done this way so that stride information can be re-used instead of re-calculated
   every time. The array is strided over based on resultType *)
let rec genArrayDeref ~arrayType ?resultType ~isMem array
  : (C.expr -> C.expr, _) GenState.u
  =
  let open GenState.Let_syntax in
  let resultType = Option.value resultType ~default:(guillotineType arrayType) in
  match arrayType with
  | Type.Array _ ->
    let resultTypeIsArray =
      match resultType with
      | Type.Array _ -> true
      | Type.Tuple _ | Type.Atom _ -> false
    in
    let resultIsPointer = isMem || resultTypeIsArray in
    let strideShape =
      match resultType with
      | Type.Array { element = _; shape = strideShape } -> NeList.to_list strideShape
      | Type.Atom _ -> []
      | Type.Tuple _ -> raise @@ Unreachable.Error "inconsistent result and array types"
    in
    let%bind stride = genShapeSize strideShape |> GenState.storeExpr ~name:"stride" in
    let stridedIndex index = Cx.(index * stride) in
    return
    @@
    if resultIsPointer
    then fun index -> Cx.(array + stridedIndex index)
    else fun index -> Cx.(arrayDeref array [ stridedIndex index ])
  | Type.Tuple elements ->
    let resultElements =
      match resultType with
      | Type.Tuple resultElements -> resultElements
      | _ -> raise @@ Unreachable.Error "Expected tuple type"
    in
    let%bind type' = genType ~wrapInPtr:isMem resultType in
    let%bind elementBuilders =
      List.zip_exn elements resultElements
      |> List.mapi ~f:(fun i (arrayElemType, resultElemType) ->
        genArrayDeref
          ~arrayType:arrayElemType
          ~resultType:resultElemType
          ~isMem
          Cx.(array %. tupleFieldName i))
      |> GenState.all
    in
    return
    @@ fun index ->
    Cx.initStruct
      type'
      (List.map elementBuilders ~f:(fun elementBuilder -> elementBuilder index))
  | Type.Atom _ -> raise @@ Unreachable.Error "Expected array type"
;;

let genMalloc
  : type l.
    hostOrDevice:l hostOrDevice
    -> store:bool
    -> memLoc:Expr.mallocLoc
    -> Type.t
    -> (C.expr, _) GenState.u
  =
  fun ~hostOrDevice ~store ~memLoc type' ->
  let open GenState.Let_syntax in
  let mallocer =
    match hostOrDevice, memLoc with
    | Host, MallocHost -> "mallocHost"
    | Host, MallocDevice -> "mallocDevice"
    | Device, MallocDevice -> "mallocDeviceOnDevice"
    | Device, MallocHost ->
      raise @@ Unreachable.Error "cannot malloc host memory from device"
  in
  let rec mallocType type' =
    match type' with
    | Type.Array { element; shape } ->
      let%bind cElementType = genType ~wrapInPtr:false (Atom element) in
      let numElements = genShapeSize @@ NeList.to_list shape in
      return @@ Cx.callBuiltin mallocer ~typeArgs:[ cElementType ] [ numElements ]
    | Type.Tuple elements ->
      let%bind cType = genType ~wrapInPtr:true (Tuple elements) in
      let%bind mallocedElements = elements |> List.map ~f:mallocType |> GenState.all in
      return @@ Cx.initStruct cType mallocedElements
    | Type.Atom _ ->
      let%bind cType = genType ~wrapInPtr:false type' in
      return @@ Cx.(callBuiltin mallocer ~typeArgs:[ cType ] [ intLit 1 ])
  in
  let%bind mem = mallocType type' in
  if store then GenState.storeExpr ~name:"mem" mem else return mem
;;

let rec genMem : store:bool -> Mem.t -> (C.expr, _) GenState.u =
  fun ~store expr ->
  let open GenState.Let_syntax in
  let storeIfRequested ~name e = if store then GenState.storeExpr ~name e else return e in
  match expr with
  | Ref { id; type' = _ } -> return @@ Cx.refId id
  | Values { elements; type' } ->
    let%bind elements = elements |> List.map ~f:(genMem ~store) |> GenState.all in
    let%bind type' = genType ~wrapInPtr:true type' in
    storeIfRequested ~name:"memValues" @@ Cx.initStruct type' elements
  | TupleDeref { tuple; index; type' = _ } ->
    let%map tuple = genMem ~store tuple in
    Cx.(tuple %. tupleFieldName index)
  | Index { mem; offset; type' } ->
    let memType = Mem.type' mem in
    let%bind offset = genDim offset |> GenState.storeExpr ~name:"offset" in
    let%bind mem = genMem ~store:true mem in
    let%bind derefer =
      genArrayDeref ~arrayType:memType ~resultType:type' ~isMem:true mem
    in
    return @@ derefer offset
;;

let genCopyExprToMem =
  let open GenState.Let_syntax in
  let rec genCopyExprToMem
    ~memNeedsPtrDeref
    ~(mem : C.expr)
    ~(expr : C.expr)
    ~(type' : Type.t)
    =
    match type' with
    | Array { element = elementType; shape } ->
      let%bind size =
        genShapeSize (NeList.to_list shape) |> GenState.storeExpr ~name:"size"
      in
      GenState.writeForLoop
        ~loopVar:"i"
        ~loopVarType:Int64
        ~initialValue:Cx.(intLit 0)
        ~cond:(fun loopVar -> Cx.(loopVar < size))
        ~loopVarUpdate:IncrementOne
        ~body:(fun loopVar ->
          genCopyExprToMem
            ~memNeedsPtrDeref:false
            ~mem:(ArrayDeref { value = mem; index = loopVar })
            ~expr:(ArrayDeref { value = expr; index = loopVar })
            ~type':(Atom elementType))
    | Tuple elements ->
      elements
      |> List.mapi ~f:(fun i element ->
        genCopyExprToMem
          ~memNeedsPtrDeref
          ~mem:Cx.(mem %. tupleFieldName i)
          ~expr:Cx.(expr %. tupleFieldName i)
          ~type':element)
      |> GenState.all_unit
    | Atom (Sigma { parameters; body = _ }) ->
      let%bind () =
        parameters
        |> List.map ~f:(fun param ->
          GenState.write
          @@ Cx.(
               fieldDeref mem (UniqueName param.binding) ~inPtr:memNeedsPtrDeref
               := expr %. UniqueName param.binding))
        |> GenState.all_unit
      in
      GenState.write
      @@ Cx.(
           fieldDeref mem boxValueFieldName ~inPtr:memNeedsPtrDeref
           := expr %. boxValueFieldName)
    | Atom (Literal (IntLiteral | FloatLiteral | BooleanLiteral | CharacterLiteral)) ->
      let mem = if memNeedsPtrDeref then C.PtrDeref mem else mem in
      GenState.write @@ Cx.(mem := expr)
  in
  genCopyExprToMem ~memNeedsPtrDeref:true
;;

let rec genCopyExprBetweenMachines ~(type' : Type.t) ~copyDir ?targetMem source =
  let open GenState.Let_syntax in
  let dirStr =
    match copyDir with
    | `HostToDevice -> "HostToDevice"
    | `DeviceToHost -> "DeviceToHost"
  in
  let writeToMem ~mem ~type' expr =
    match copyDir with
    | `HostToDevice ->
      let%bind exprVar = GenState.createVarAuto "value" expr in
      let%bind type' = genType type' in
      GenState.write
      @@ C.Eval
           Cx.(
             callBuiltin
               "copyHostToDevice"
               ~typeArgs:[ type' ]
               [ mem; PtrRef exprVar; intLit 1 ])
    | `DeviceToHost -> GenState.write Cx.(PtrDeref mem := expr)
  in
  match type' with
  | Array { element; shape } ->
    let size = genShapeSize @@ NeList.to_list shape in
    let%bind cElementType = genType ~wrapInPtr:false (Atom element) in
    (match targetMem with
     | None ->
       return
         Cx.(
           callBuiltin
             [%string "copy%{dirStr}Malloc"]
             ~typeArgs:[ cElementType ]
             [ source; size ])
     | Some target ->
       let%bind () =
         GenState.write
         @@ C.Eval
              Cx.(
                callBuiltin
                  [%string "copy%{dirStr}"]
                  ~typeArgs:[ cElementType ]
                  [ target; source; size ])
       in
       return target)
  | Tuple elements ->
    (match targetMem with
     | None ->
       let%bind cType = genType type' in
       let%bind args =
         elements
         |> List.mapi ~f:(fun i elementType ->
           genCopyExprBetweenMachines
             ~type':elementType
             ~copyDir
             Cx.(source %. tupleFieldName i))
         |> GenState.all
       in
       return @@ Cx.initStruct cType args
     | Some target ->
       let%bind _ =
         elements
         |> List.mapi ~f:(fun i elementType ->
           genCopyExprBetweenMachines
             ~type':elementType
             ~copyDir
             ~targetMem:Cx.(target %. tupleFieldName i)
             Cx.(source %. tupleFieldName i))
         |> GenState.all
       in
       return target)
  | Atom (Literal _) ->
    let%bind () =
      match targetMem with
      | Some target -> writeToMem ~mem:target ~type' source
      | None -> return ()
    in
    return source
  | Atom (Sigma { parameters; body }) ->
    let%bind copiedParams =
      parameters
      |> List.map ~f:(fun param ->
        let sourceValue = Cx.(source %. UniqueName param.binding) in
        let value =
          match param.bound with
          | Dim -> sourceValue
          | Shape ->
            let dimCount = Cx.(sourceValue %. sliceDimCountFieldName) in
            Cx.(
              initStruct
                (TypeRef (StrName "Slice"))
                [ callBuiltin
                    [%string "copy%{dirStr}Malloc"]
                    ~typeArgs:[ Int64 ]
                    [ source %. sliceDimsFieldName; dimCount ]
                ; dimCount
                ])
        in
        let%bind () =
          GenState.write
          @@ Define { name = UniqueName param.binding; type' = None; value = Some value }
        in
        return @@ Cx.(refId param.binding))
      |> GenState.all
    in
    let%bind copiedValue =
      genCopyExprBetweenMachines ~type':body ~copyDir Cx.(source %. boxValueFieldName)
    in
    let%bind cType = genType type' in
    let%bind copiedBox =
      GenState.createVarAuto "box" @@ Cx.initStruct cType (copiedValue :: copiedParams)
    in
    let%bind () =
      match targetMem with
      | Some target -> writeToMem ~mem:target ~type' copiedBox
      | None -> return ()
    in
    return copiedBox
;;

let rec genMoveMem direction ~(type' : Type.t) ~target ~source =
  let open GenState.Let_syntax in
  let copyFun =
    match direction with
    | `DeviceToHost -> "copyDeviceToHost"
    | `HostToDevice -> "copyHostToDevice"
    | `HostToHost -> "copyHostToHost"
    | `DeviceToDevice -> "copyDeviceToDevice"
  in
  match type' with
  | Array { element; shape } ->
    let size = genShapeSize @@ NeList.to_list shape in
    let%bind cElementType = genType ~wrapInPtr:false (Atom element) in
    GenState.write
    @@ C.Eval Cx.(callBuiltin copyFun ~typeArgs:[ cElementType ] [ target; source; size ])
  | Tuple elements ->
    elements
    |> List.mapi ~f:(fun i elementType ->
      genMoveMem
        direction
        ~type':elementType
        ~target:Cx.(target %. tupleFieldName i)
        ~source:Cx.(source %. tupleFieldName i))
    |> GenState.all_unit
  | Atom (Literal _) ->
    let%bind cType = genType ~wrapInPtr:false type' in
    GenState.write
    @@ C.Eval Cx.(callBuiltin copyFun ~typeArgs:[ cType ] [ target; source; intLit 1 ])
  | Atom (Sigma { parameters; body }) ->
    let%bind () =
      parameters
      |> List.map ~f:(fun param ->
        let sourceValue = Cx.(source %. UniqueName param.binding) in
        let value =
          match param.bound with
          | Dim -> sourceValue
          | Shape ->
            let dimCount = Cx.(sourceValue %. sliceDimCountFieldName) in
            Cx.(
              initStruct
                (TypeRef (StrName "Slice"))
                [ callBuiltin
                    copyFun
                    ~typeArgs:[ Int64 ]
                    [ source %. sliceDimsFieldName; dimCount ]
                ; dimCount
                ])
        in
        let%bind () =
          GenState.write
          @@ Define { name = UniqueName param.binding; type' = None; value = Some value }
        in
        GenState.write @@ Cx.(target %-> UniqueName param.binding := refId param.binding))
      |> GenState.all_unit
    in
    let%bind copiedBoxValue =
      let boxValue = Cx.(source %. boxValueFieldName) in
      match direction with
      | (`HostToDevice | `DeviceToHost) as direction ->
        genCopyExprBetweenMachines ~type':body ~copyDir:direction boxValue
      | `HostToHost | `DeviceToDevice -> return @@ boxValue
    in
    GenState.write Cx.(target %-> boxValueFieldName := copiedBoxValue)
;;

type funPass =
  { arg : C.expr
  ; param : C.funParam
  }
[@@deriving sexp_of]

let handleCaptures Expr.{ indexCaptures; exprCaptures; memCaptures }
  : (funPass list, _) GenState.u
  =
  let open GenState.Let_syntax in
  let indexPasses =
    indexCaptures
    |> Map.to_alist
    |> List.map ~f:(fun (id, sort) ->
      let cRef = Cx.(refId id) in
      match sort with
      | Dim ->
        { arg = cRef; param = ({ name = UniqueName id; type' = Int64 } : C.funParam) }
      | Shape ->
        let dimCount = Cx.(cRef %. sliceDimCountFieldName) in
        let dims = Cx.(cRef %. sliceDimsFieldName) in
        let arg =
          Cx.(
            initStruct
              (TypeRef (StrName "Slice"))
              [ callBuiltin "copyHostToDevice" ~typeArgs:[ Int64 ] [ dims; dimCount ]
              ; dimCount
              ])
        in
        let param : C.funParam = { name = UniqueName id; type' = Int64 } in
        { arg; param })
  in
  let%bind exprPasses =
    exprCaptures
    |> Map.to_alist
    |> List.map ~f:(fun (id, type') ->
      let%bind cType = genType type' in
      let%bind arg =
        genCopyExprBetweenMachines ~type' ~copyDir:`HostToDevice Cx.(refId id)
      in
      let param : C.funParam = { name = UniqueName id; type' = cType } in
      return { arg; param })
    |> GenState.all
  in
  let%bind memPasses =
    memCaptures
    |> Map.to_alist
    |> List.map ~f:(fun (id, type') ->
      let%bind cType = genType ~wrapInPtr:true type' in
      let param : C.funParam = { name = UniqueName id; type' = cType } in
      return { arg = Cx.refId id; param })
    |> GenState.all
  in
  let paramsAndArgs = indexPasses @ exprPasses @ memPasses in
  return paramsAndArgs
;;

let rec genMax ~store l ~default =
  let open GenState.Let_syntax in
  let storeIfRequested e = if store then GenState.storeExpr ~name:"max" e else return e in
  match l with
  | [] -> return default
  | [ value ] -> return value
  | head :: rest ->
    let%bind maxRest = genMax ~store:true rest ~default in
    storeIfRequested @@ Cx.(ternary ~cond:(head > maxRest) ~then':head ~else':maxRest)
;;

let genIota ~loopVar ~loopSize ({ iota; nestIn } : Expr.mapIota) =
  let value =
    match nestIn with
    | None -> loopVar
    | Some parentIota -> Cx.((refId parentIota * loopSize) + loopVar)
  in
  GenState.write
  @@ Define { name = UniqueName iota; type' = Some Int64; value = Some value }
;;

let rec genMatchMapBody (matcher : Expr.tupleMatch) res =
  match matcher with
  | Binding id ->
    GenState.write @@ C.Define { name = UniqueName id; type' = None; value = Some res }
  | Unpack matchers ->
    matchers
    |> List.mapi ~f:(fun i matcher ->
      genMatchMapBody matcher @@ Cx.(res %. tupleFieldName i))
    |> GenState.all_unit
;;

let rec genValueOfProductionTuple : Expr.productionTuple -> (C.expr, _) GenState.u =
  let open GenState.Let_syntax in
  function
  | ProductionTuple { elements; type' } ->
    let%bind cType = genType type' in
    let%bind args = elements |> List.map ~f:genValueOfProductionTuple |> GenState.all in
    return @@ Cx.initStruct cType args
  | ProductionTupleAtom production -> return @@ Cx.(refId production.productionId)
;;

let createFunPass varName type' hostValue =
  let open GenState.Let_syntax in
  let%bind paramVar =
    GenState.createName @@ NameOfStr { str = varName; needsUniquifying = true }
  in
  return (C.VarRef paramVar, { arg = hostValue; param = { name = paramVar; type' } })
;;

let rec genStmnt
  : type l.
    hostOrDevice:l hostOrDevice
    -> (l, Expr.captures) Expr.statement
    -> (unit, _) GenState.u
  =
  fun ~hostOrDevice stmnt ->
  let open GenState.Let_syntax in
  match hostOrDevice, stmnt with
  | _, Statements statements ->
    statements |> List.map ~f:(genStmnt ~hostOrDevice) |> GenState.all_unit
  | _, Putmem { addr; expr; type' } ->
    let%bind addr = genMem ~store:true addr in
    let%bind expr = genExpr ~hostOrDevice ~store:true expr in
    genCopyExprToMem ~expr ~mem:addr ~type'
  | Host, MapKernel { kernel; captures; blocks; threads } ->
    let%bind capturePasses = handleCaptures captures in
    let%bind resultInterim = genMem ~store:true kernel.mapResultMemDeviceInterim in
    let module Map = struct
      type hostAndDeviceExpr =
        { host : C.expr
        ; device : C.expr
        }
      [@@deriving sexp_of]

      (* Represent map kernels that are annotated with some re-used values *)
      type body =
        | Statement of (device, Expr.captures) Expr.statement
        | SubMaps of
            { subMaps : t list
            ; maxBodySize : hostAndDeviceExpr
            }

      and t =
        { frameShapeSize : hostAndDeviceExpr
        ; mapArgs : Expr.mapArg list
        ; mapMemArgs : Expr.memArg list
        ; mapIotas : Expr.mapIota list
        ; mapBody : body
        ; bodySize : hostAndDeviceExpr
        }
      [@@deriving sexp_of]
    end
    in
    let hostAndDeviceExprFromHost ~name ~type' hostExpr =
      let%map deviceVar =
        GenState.createName @@ NameOfStr { str = name; needsUniquifying = true }
      in
      ( Map.{ host = hostExpr; device = VarRef deviceVar }
      , { arg = hostExpr; param = { name = deviceVar; type' } } )
    in
    let rec annotateMapKernel
      Expr.{ frameShape; mapArgs; mapMemArgs; mapIotas; mapBody; type' = _ }
      =
      let%bind frameShapeSize, frameShapeSizePass =
        genShapeElementSize frameShape
        |> GenState.storeExpr ~name:"frameShapeSize"
        >>= hostAndDeviceExprFromHost ~name:"frameShapeSize" ~type':Int64
      in
      let%bind mapBody, bodySize, passesFromBody =
        match mapBody with
        | MapBodyStatement statement ->
          return (Map.Statement statement, frameShapeSize, [])
        | MapBodySubMaps subMaps ->
          let%bind subMaps, passes =
            subMaps |> List.map ~f:annotateMapKernel |> GenState.all >>| List.unzip
          in
          let%bind maxBodySize, maxBodySizePass =
            subMaps
            |> List.map ~f:(fun (subMap : Map.t) -> subMap.bodySize.host)
            |> genMax ~store:false ~default:(Cx.intLit 1)
            >>= hostAndDeviceExprFromHost ~name:"maxBodySize" ~type':Int64
          in
          let%bind bodySize, bodySizePass =
            Cx.(maxBodySize.host * frameShapeSize.host)
            |> GenState.storeExpr ~name:"bodySize"
            >>= hostAndDeviceExprFromHost ~name:"bodySize" ~type':Int64
          in
          return
            ( Map.SubMaps { subMaps; maxBodySize }
            , bodySize
            , maxBodySizePass :: bodySizePass :: List.concat passes )
      in
      return
        ( Map.{ frameShapeSize; mapArgs; mapMemArgs; mapIotas; mapBody; bodySize }
        , frameShapeSizePass :: passesFromBody )
    in
    let%bind annotatedMapKernel, mapKernelPasses = annotateMapKernel kernel.map in
    let kernelPasses = capturePasses @ mapKernelPasses in
    let rec genMapBody
      chunkVar
      Map.{ frameShapeSize; mapArgs; mapMemArgs; mapIotas; mapBody; bodySize = _ }
      =
      let%bind loopVar =
        match mapBody with
        | Statement _ -> return chunkVar
        | SubMaps { subMaps = _; maxBodySize } ->
          GenState.storeExpr ~name:"i" @@ Cx.(chunkVar / maxBodySize.device)
      in
      GenState.writeIte
        ~cond:Cx.(loopVar < frameShapeSize.device)
        ~thenBranch:
          (let%bind () =
             genMapBodySetup
               ~loopVar
               ~loopSize:frameShapeSize.device
               ~mapArgs
               ~mapIotas
               ~mapMemArgs
           in
           match mapBody with
           | Statement statement -> genStmnt ~hostOrDevice:Device statement
           | SubMaps { subMaps; maxBodySize } ->
             subMaps
             |> List.map ~f:(genMapBody Cx.(chunkVar % maxBodySize.device))
             |> GenState.all_unit)
        ~elseBranch:(return ())
    in
    let%bind kernelName =
      GenState.defineFun
        (NameOfStr { str = "mapKernel"; needsUniquifying = true })
        ~f:(fun _ ->
          let%bind body =
            GenState.block
            @@
            let%bind () =
              GenState.writeForLoop
                ~loopVar:"i"
                ~loopVarType:Int64
                ~initialValue:Cx.((blockIndex * intLit threads) + threadIndex)
                ~cond:(fun loopVar -> Cx.(loopVar < annotatedMapKernel.bodySize.device))
                ~loopVarUpdate:(Increment (Cx.intLit @@ (blocks * threads)))
                ~body:(fun loopVar -> genMapBody loopVar annotatedMapKernel)
            in
            return ()
          in
          return
          @@ C.
               { params = List.map kernelPasses ~f:(fun pass -> pass.param)
               ; body
               ; returnType = None
               ; funType = Kernel
               })
    in
    let%bind () =
      GenState.write
      @@ C.Eval
           (KernelLaunch
              { kernel = kernelName
              ; blocks = Cx.intLit blocks
              ; threads = Cx.intLit threads
              ; args = List.map kernelPasses ~f:(fun pass -> pass.arg)
              })
    in
    let%bind resultFinal = genMem ~store:true kernel.mapResultMemHostFinal in
    let%bind () =
      genMoveMem
        `DeviceToHost
        ~type':(Mem.type' kernel.mapResultMemHostFinal)
        ~target:resultFinal
        ~source:resultInterim
    in
    return ()
  | _, ComputeForSideEffects expr ->
    let%bind expr = genExpr ~hostOrDevice ~store:false expr in
    GenState.write @@ C.Eval expr
  | _, SLet let' -> genLet ~hostOrDevice ~genBody:(genStmnt ~hostOrDevice) let'
  | _, SMallocLet mallocLet ->
    genMallocLet ~hostOrDevice ~genBody:(genStmnt ~hostOrDevice) mallocLet
  | _, ReifyShapeIndex { shape; mem } ->
    let%bind mem = genMem ~store:true mem in
    reifyShapeIndexToMem ~mem shape >>| fun _ -> ()

and genExpr
  : type l.
    hostOrDevice:l hostOrDevice
    -> store:bool
    -> l Expr.withCaptures
    -> (C.expr, _) GenState.u
  =
  fun ~hostOrDevice ~store expr ->
  let open GenState.Let_syntax in
  let storeIfRequested ~name e = if store then GenState.storeExpr ~name e else return e in
  match hostOrDevice, expr with
  | _, Box { indices; body; type' } ->
    let%bind boxType = genType (Atom (Sigma type')) in
    GenState.scope "box" boxType
    @@
    let indicesAndParams = List.zip_exn indices type'.parameters in
    let%bind indices =
      indicesAndParams
      |> List.map ~f:(fun (index, param) ->
        let%bind type' = genType (Expr.type' index.expr) in
        let%bind indexExpr = genExpr ~hostOrDevice ~store:false index.expr in
        let index =
          match index.index with
          | Shape shape -> Cx.initStruct type' [ indexExpr; genShapeDimCount shape ]
          | Dimension _ -> indexExpr
        in
        let paramName = C.Name.UniqueName param.binding in
        let%bind () =
          GenState.write
          @@ C.Define { name = paramName; type' = Some type'; value = Some index }
        in
        return @@ C.VarRef paramName)
      |> GenState.all
    in
    let%bind body = genExpr ~hostOrDevice ~store:false body in
    return @@ Cx.initStruct boxType (body :: indices)
  | _, Literal (IntLiteral i) -> return @@ Cx.(intLit i)
  | _, Literal (FloatLiteral f) -> return @@ C.Literal (Float64Literal f)
  | _, Literal (CharacterLiteral c) -> return @@ C.Literal (CharLiteral c)
  | _, Literal (BooleanLiteral b) -> return @@ C.Literal (BoolLiteral b)
  | _, ScalarPrimitive { op; args; type' } ->
    let genBinop binop =
      let%bind args =
        args |> List.map ~f:(genExpr ~hostOrDevice ~store:false) |> GenState.all
      in
      let arg1, arg2 =
        match args with
        | [ arg1; arg2 ] -> arg1, arg2
        | _ -> raise @@ Unreachable.Error "expected two args for binop"
      in
      storeIfRequested ~name:"binopResult" @@ C.Binop { op = binop; arg1; arg2 }
    in
    let genCast type' =
      let%bind args =
        args |> List.map ~f:(genExpr ~hostOrDevice ~store:false) |> GenState.all
      in
      let arg =
        match args with
        | [ arg ] -> arg
        | _ -> raise @@ Unreachable.Error "expected one arg for cast"
      in
      storeIfRequested ~name:"castResult" @@ C.Cast { type'; value = arg }
    in
    (match op with
     | Add | AddF -> genBinop "+"
     | Sub | SubF -> genBinop "-"
     | Mul | MulF -> genBinop "*"
     | Div | DivF -> genBinop "/"
     | Mod -> genBinop "%"
     | And -> genBinop "&&"
     | Or -> genBinop "||"
     | IntToBool -> genCast Bool
     | BoolToInt -> genCast Int64
     | IntToFloat -> genCast Float64
     | FloatToInt -> genCast Int64
     | Equal -> genBinop "=="
     | Ne -> genBinop "!="
     | Gt | GtF -> genBinop ">"
     | GtEq | GtEqF -> genBinop ">="
     | Lt | LtF -> genBinop "<"
     | LtEq | LtEqF -> genBinop "<="
     | Not ->
       let arg =
         match args with
         | [ arg ] -> arg
         | _ -> raise @@ Unreachable.Error "expected one args for not"
       in
       let%bind arg = genExpr ~hostOrDevice ~store:false arg in
       storeIfRequested ~name:"castResult" @@ Cx.(not arg)
     | LibFun { name; libName; argTypes = _; retType = _ } ->
       let%bind args =
         args |> List.map ~f:(genExpr ~hostOrDevice ~store:false) |> GenState.all
       in
       storeIfRequested ~name:[%string "%{name}Result"] @@ Cx.callBuiltin libName args
     | If ->
       let cond, then', else' =
         match args with
         | [ cond; then'; else' ] -> cond, then', else'
         | _ -> raise @@ Unreachable.Error "expected three args for if"
       in
       let%bind cType = genType type' in
       let%bind res =
         GenState.createVar "ifResult"
         @@ fun name -> C.Define { name; type' = Some cType; value = None }
       in
       let%bind cond = genExpr ~hostOrDevice ~store:false cond in
       let%bind () =
         GenState.writeIte
           ~cond
           ~thenBranch:
             (let%bind then' = genExpr ~hostOrDevice ~store:false then' in
              GenState.write @@ Cx.(res := then'))
           ~elseBranch:
             (let%bind else' = genExpr ~hostOrDevice ~store:false else' in
              GenState.write @@ Cx.(res := else'))
       in
       return res)
  | _, TupleDeref { tuple; index; type' = _ } ->
    let%map tuple = genExpr ~hostOrDevice ~store tuple in
    Cx.(tuple %. tupleFieldName index)
  | _, Values { elements; type' } ->
    let%bind elements =
      elements |> List.map ~f:(genExpr ~hostOrDevice ~store:false) |> GenState.all
    in
    let%bind type' = genType (Tuple type') in
    storeIfRequested ~name:"values" @@ Cx.initStruct type' elements
  | _, Ref { id; type' = _ } -> return @@ Cx.(refId id)
  | _, BoxValue { box; type' = _ } ->
    let%bind box = genExpr ~hostOrDevice ~store box in
    return Cx.(box %. boxValueFieldName)
  | _, IndexLet { indexArgs; body; type' = _ } ->
    let%bind () =
      indexArgs
      |> List.map ~f:(fun { indexBinding; indexValue; sort } ->
        let%bind indexValue =
          match indexValue with
          | Runtime v ->
            let%map cv = genExpr ~hostOrDevice ~store:false v in
            (match sort with
             | Dim -> cv
             | Shape ->
               let size =
                 match Expr.type' v with
                 | Array { element = Literal IntLiteral; shape = [ size ] } -> size
                 | _ -> raise @@ Unreachable.Error "expected 1d array of ints"
               in
               let dimCount = genShapeElementSize size in
               Cx.initStruct (genTypeForSort Shape) [ cv; dimCount ])
          | FromBox { box; i } ->
            let params =
              match Expr.type' box with
              | Atom (Sigma sigma) -> sigma.parameters
              | _ -> raise @@ Unreachable.Error "expected sigma type"
            in
            let param = List.nth_exn params i in
            let%map box = genExpr ~hostOrDevice ~store:false box in
            Cx.(box %. UniqueName param.binding)
        in
        let type' =
          match sort with
          | Dim -> C.Int64
          | Shape -> C.Ptr Int64
        in
        GenState.write
        @@ C.Define
             { name = UniqueName indexBinding
             ; type' = Some type'
             ; value = Some indexValue
             })
      |> GenState.all_unit
    in
    genExpr ~hostOrDevice ~store body
  | _, Let let' -> genLet ~hostOrDevice ~genBody:(genExpr ~hostOrDevice ~store) let'
  | _, MallocLet mallocLet ->
    genMallocLet ~hostOrDevice ~genBody:(genExpr ~hostOrDevice ~store) mallocLet
  | _, ReifyDimensionIndex { dim } -> GenState.storeExpr ~name:"reifiedDim" @@ genDim dim
  | ( _
    , LoopBlock
        { frameShape
        ; mapArgs
        ; mapMemArgs
        ; mapIotas
        ; mapBody
        ; mapBodyMatcher
        ; mapResults (* mapResultMemInterim = mapResultMemFinal for LoopBlock *)
        ; mapResultMemFinal = mapResultMem
        ; consumer
        ; type'
        } ) ->
    let%bind steps = genShapeElementSize frameShape |> GenState.storeExpr ~name:"steps" in
    let%bind mapArgsWithDerefers =
      mapArgs
      |> List.map ~f:(fun arg ->
        let%bind argRef = genExpr ~hostOrDevice ~store:true @@ Ref arg.ref in
        let%bind derefer = genArrayDeref ~arrayType:arg.ref.type' ~isMem:false argRef in
        return @@ (arg, derefer))
      |> GenState.all
    in
    let%bind mapMemArgsWithDerefers =
      mapMemArgs
      |> List.map ~f:(fun arg ->
        let%bind argRef = genMem ~store:true arg.mem in
        let%bind derefer =
          genArrayDeref ~arrayType:(Mem.type' arg.mem) ~isMem:true argRef
        in
        return @@ (arg, derefer))
      |> GenState.all
    in
    let%bind cMapResultMem = genMem ~store:true mapResultMem in
    let mapResultTypes =
      match type' with
      | [ Tuple mapResultTypes; _ ] -> mapResultTypes
      | _ ->
        raise
        @@ Unreachable.Error "expected 2 element tuple where first element is a tuple"
    in
    let%bind mapResultDerefersAndTypes =
      mapResultTypes
      |> List.mapi ~f:(fun i resultTypeArr ->
        let%bind derefer =
          genArrayDeref
            ~arrayType:resultTypeArr
            ~isMem:true
            Cx.(cMapResultMem %. tupleFieldName i)
        in
        return @@ (derefer, guillotineType resultTypeArr))
      |> GenState.all
    in
    let%bind loopVar =
      GenState.createName (NameOfStr { str = "i"; needsUniquifying = true })
    in
    let%bind consumerInLoop, consumerResult, loopInReverse =
      match consumer with
      | Nothing ->
        let%bind unitType = genType @@ Tuple [] in
        return @@ (return (), Cx.initStruct unitType [], false)
      | Just (ReduceSeq { arg; zero; body; d = _; type' = _ }) ->
        let accVar = C.Name.UniqueName arg.firstBinding in
        let stepVar = C.Name.UniqueName arg.secondBinding in
        let%bind accType = genType @@ Expr.type' body in
        let%bind cZero = genExpr ~hostOrDevice ~store:false zero in
        let%bind () =
          GenState.write @@ C.Define { name = accVar; type' = None; value = Some cZero }
        in
        let inLoop =
          let%bind stepValue = genValueOfProductionTuple arg.production in
          let%bind () =
            GenState.write
            @@ C.Define { name = stepVar; type' = Some accType; value = Some stepValue }
          in
          let%bind body = genExpr ~hostOrDevice ~store:false body in
          GenState.write Cx.(VarRef accVar := body)
        in
        return @@ (inLoop, C.VarRef accVar, false)
      | Just (ScanSeq { arg; zero; body; d = _; scanResultMemFinal; type' = _ }) ->
        let accVar = C.Name.UniqueName arg.firstBinding in
        let stepVar = C.Name.UniqueName arg.secondBinding in
        let%bind accType = genType @@ Expr.type' body in
        let%bind cZero = genExpr ~hostOrDevice ~store:false zero in
        let%bind () =
          GenState.write @@ C.Define { name = accVar; type' = None; value = Some cZero }
        in
        let%bind cMem = genMem ~store:true scanResultMemFinal in
        let%bind memDerefer =
          genArrayDeref ~arrayType:(Mem.type' scanResultMemFinal) ~isMem:true cMem
        in
        let%bind () =
          genCopyExprToMem
            ~mem:(memDerefer (Literal (Int64Literal 0)))
            ~expr:(VarRef accVar)
            ~type':(Expr.type' body)
        in
        let inLoop =
          let%bind stepValue = genValueOfProductionTuple arg.production in
          let%bind () =
            GenState.write
            @@ C.Define { name = stepVar; type' = Some accType; value = Some stepValue }
          in
          let%bind cBody = genExpr ~hostOrDevice ~store:false body in
          let%bind () = GenState.write @@ Cx.(VarRef accVar := cBody) in
          genCopyExprToMem
            ~mem:(memDerefer Cx.(VarRef loopVar + intLit 1))
            ~expr:(VarRef accVar)
            ~type':(Expr.type' body)
        in
        return @@ (inLoop, cMem, false)
      | Just
          (Fold
            { arrayArgs
            ; zeroArg
            ; mappedMemArgs
            ; body
            ; reverse
            ; d = _
            ; character
            ; type' = _
            }) ->
        let accVar = C.Name.UniqueName zeroArg.zeroBinding in
        let%bind accType = genType @@ Expr.type' body in
        let%bind cZero = genExpr ~hostOrDevice ~store:false zeroArg.zeroValue in
        let%bind () =
          GenState.write
          @@ C.Define { name = accVar; type' = Some accType; value = Some cZero }
        in
        let%bind foldMemArgsWithDerefers =
          mappedMemArgs
          |> List.map ~f:(fun arg ->
            let%bind argRef = genMem ~store:true arg.mem in
            let%bind derefer =
              genArrayDeref ~arrayType:(Mem.type' arg.mem) ~isMem:true argRef
            in
            return @@ (arg, derefer))
          |> GenState.all
        in
        let%bind characterInLoop, characterResult =
          match character with
          | Fold -> return @@ (return (), C.VarRef accVar)
          | Trace mem ->
            let%bind cMem = genMem ~store:true mem in
            let%bind memDerefer =
              genArrayDeref ~arrayType:(Mem.type' mem) ~isMem:true cMem
            in
            let%bind () =
              genCopyExprToMem
                ~mem:(memDerefer @@ if reverse then steps else Cx.intLit 0)
                ~expr:(VarRef accVar)
                ~type':(Expr.type' body)
            in
            let inLoop =
              genCopyExprToMem
                ~mem:
                  (memDerefer
                   @@ if reverse then VarRef loopVar else Cx.(VarRef loopVar + intLit 1))
                ~expr:(VarRef accVar)
                ~type':(Expr.type' body)
            in
            return (inLoop, cMem)
        in
        let inLoop =
          let%bind () =
            arrayArgs
            |> List.map ~f:(fun { binding; production } ->
              let productionValue = Cx.(refId production.productionId) in
              GenState.write
              @@ C.Define
                   { name = UniqueName binding
                   ; type' = None
                   ; value = Some productionValue
                   })
            |> GenState.all_unit
          in
          let%bind () =
            foldMemArgsWithDerefers
            |> List.map ~f:(fun (arg, derefer) ->
              GenState.write
              @@ C.Define
                   { name = UniqueName arg.memBinding
                   ; type' = None
                   ; value = Some (derefer (VarRef loopVar))
                   })
            |> GenState.all_unit
          in
          let%bind body = genExpr ~hostOrDevice ~store:false body in
          let%bind () = GenState.write Cx.(VarRef accVar := body) in
          characterInLoop
        in
        return @@ (inLoop, characterResult, reverse)
      | Just
          (Scatter
            { valuesArg
            ; indicesArg
            ; memInterim = _
            ; memFinal = mem
            ; dIn = _
            ; dOut = _
            ; type' = _
            }) ->
        (* For sequential, memInterim = memFinal, so can just write directly to memFinal *)
        let%bind cMem = genMem ~store:true mem in
        let cValueArg = Cx.(refId valuesArg.productionId) in
        let cIndexArg = Cx.(refId indicesArg.productionId) in
        let%bind outputDerefer =
          genArrayDeref ~arrayType:(Mem.type' mem) ~isMem:true cMem
        in
        let inLoop =
          GenState.writeIte
            ~cond:Cx.(cIndexArg >= intLit 0)
            ~thenBranch:
              (genCopyExprToMem
                 ~mem:(outputDerefer cIndexArg)
                 ~expr:cValueArg
                 ~type':(guillotineType @@ Mem.type' mem))
            ~elseBranch:(return ())
        in
        return @@ (inLoop, cMem, false)
    in
    let%bind body =
      GenState.block
      @@
      let%bind () =
        mapArgsWithDerefers
        |> List.map ~f:(fun (arg, derefer) ->
          GenState.write
          @@ C.Define
               { name = UniqueName arg.binding
               ; type' = None
               ; value = Some (derefer (VarRef loopVar))
               })
        |> GenState.all_unit
      in
      let%bind () =
        mapMemArgsWithDerefers
        |> List.map ~f:(fun (arg, derefer) ->
          GenState.write
          @@ C.Define
               { name = UniqueName arg.memBinding
               ; type' = None
               ; value = Some (derefer (VarRef loopVar))
               })
        |> GenState.all_unit
      in
      let%bind () =
        mapIotas
        |> List.map ~f:(genIota ~loopVar:(VarRef loopVar) ~loopSize:steps)
        |> GenState.all_unit
      in
      let%bind mapRes = genExpr ~hostOrDevice ~store:true mapBody in
      let%bind () = genMatchMapBody mapBodyMatcher mapRes in
      let%bind () =
        List.zip_exn mapResults mapResultDerefersAndTypes
        |> List.map ~f:(fun (resultId, (resultDerefer, resultType)) ->
          genCopyExprToMem
            ~expr:Cx.(refId resultId)
            ~mem:(resultDerefer @@ VarRef loopVar)
            ~type':resultType)
        |> GenState.all_unit
      in
      consumerInLoop
    in
    let%bind () =
      GenState.write
      @@
      if loopInReverse
      then
        C.ForLoop
          { loopVar
          ; loopVarType = Int64
          ; initialValue = Cx.(steps - intLit 1)
          ; cond = Cx.(VarRef loopVar >= intLit 0)
          ; loopVarUpdate = DecrementOne
          ; body
          }
      else
        C.ForLoop
          { loopVar
          ; loopVarType = Int64
          ; initialValue = Cx.(intLit 0)
          ; cond = Cx.(VarRef loopVar < steps)
          ; loopVarUpdate = IncrementOne
          ; body
          }
    in
    let%bind mapResult =
      genExpr ~hostOrDevice ~store
      @@ Getmem { addr = mapResultMem; type' = Mem.type' mapResultMem }
    in
    let%bind type' = genType @@ Tuple type' in
    return @@ Cx.initStruct type' [ mapResult; consumerResult ]
  | ( Host
    , LoopKernel
        { kernel =
            { loopBlock =
                { frameShape = _
                ; mapArgs
                ; mapMemArgs
                ; mapIotas
                ; mapBody
                ; mapBodyMatcher
                ; mapResults
                ; mapResultMemFinal
                ; consumer =
                    Just
                      (Scatter
                        { valuesArg
                        ; indicesArg
                        ; dIn
                        ; dOut = _
                        ; memInterim = scatterResultMemInterim
                        ; memFinal = scatterResultMemFinal
                        ; type' = _
                        })
                ; type'
                }
            ; mapResultMemDeviceInterim
            }
        ; captures
        ; blocks
        ; threads
        } ) ->
    let%bind capturePasses = handleCaptures captures in
    let%bind dIn, dInPass = createFunPass "dIn" Int64 @@ genDim dIn in
    let%bind cMapResultMemInterimHost = genMem ~store:true mapResultMemDeviceInterim in
    let%bind cMapResultMemInterimType =
      genType ~wrapInPtr:true @@ Mem.type' mapResultMemDeviceInterim
    in
    let%bind cMapResultMemInterimDevice, mapResultMemInterimPass =
      createFunPass
        "mapResultMemInterim"
        cMapResultMemInterimType
        cMapResultMemInterimHost
    in
    let%bind cScatterResultMemInterimHost = genMem ~store:true scatterResultMemInterim in
    let%bind cScatterResultMemInterimType =
      genType ~wrapInPtr:true @@ Mem.type' scatterResultMemInterim
    in
    let%bind cScatterResultMemInterimDevice, scatterResultMemInterimPass =
      createFunPass
        "scatterResultMemInterim"
        cScatterResultMemInterimType
        cScatterResultMemInterimHost
    in
    let kernelPasses =
      capturePasses @ [ dInPass; mapResultMemInterimPass; scatterResultMemInterimPass ]
    in
    let%bind kernelName =
      GenState.defineFun
        (NameOfStr { str = "scatterKernel"; needsUniquifying = true })
        ~f:(fun _ ->
          let%bind body =
            GenState.block
            @@
            let%bind () =
              GenState.writeForLoop
                ~loopVar:"i"
                ~loopVarType:Int64
                ~initialValue:Cx.((blockIndex * intLit threads) + threadIndex)
                ~cond:(fun i -> Cx.(i < dIn))
                ~loopVarUpdate:(Increment (Cx.intLit @@ (blocks * threads)))
                ~body:(fun loopVar ->
                  let%bind () = GenState.comment "perform the map" in
                  let%bind () =
                    genMapBodyOnDevice
                      ~loopVar
                      ~loopSize:dIn
                      ~mapArgs
                      ~mapIotas
                      ~mapMemArgs
                      ~mapBody
                      ~mapBodyMatcher
                      ~mapResults
                      ~mapResultTypes:
                        (match type' with
                         | [ Tuple mapResultTypes; _ ] -> mapResultTypes
                         | _ ->
                           raise
                           @@ Unreachable.Error
                                "expected 2 element tuple where first element is a tuple")
                      ~mapResultMem:cMapResultMemInterimDevice
                  in
                  let%bind () = GenState.comment "perform the scatter" in
                  let cValueArg = Cx.(refId valuesArg.productionId) in
                  let cIndexArg = Cx.(refId indicesArg.productionId) in
                  let%bind scatterResultMemInterimDerefer =
                    genArrayDeref
                      ~arrayType:(Mem.type' scatterResultMemInterim)
                      ~isMem:true
                      cScatterResultMemInterimDevice
                  in
                  GenState.writeIte
                    ~cond:Cx.(cIndexArg >= intLit 0)
                    ~thenBranch:
                      (genCopyExprToMem
                         ~mem:(scatterResultMemInterimDerefer cIndexArg)
                         ~expr:cValueArg
                         ~type':(guillotineType @@ Mem.type' scatterResultMemInterim))
                    ~elseBranch:(return ()))
            in
            return ()
          in
          return
          @@ C.
               { params = List.map kernelPasses ~f:(fun pass -> pass.param)
               ; body
               ; returnType = None
               ; funType = Kernel
               })
    in
    let%bind () =
      GenState.write
      @@ C.Eval
           (KernelLaunch
              { kernel = kernelName
              ; blocks = Cx.intLit blocks
              ; threads = Cx.intLit threads
              ; args = List.map kernelPasses ~f:(fun pass -> pass.arg)
              })
    in
    let%bind mapResultFinal = genMem ~store:true mapResultMemFinal in
    let%bind () =
      genMoveMem
        `DeviceToHost
        ~type':(Mem.type' mapResultMemFinal)
        ~target:mapResultFinal
        ~source:cMapResultMemInterimHost
    in
    let%bind scatterResultFinal = genMem ~store:true scatterResultMemFinal in
    let%bind () =
      genMoveMem
        `DeviceToHost
        ~type':(Mem.type' scatterResultMemFinal)
        ~target:scatterResultFinal
        ~source:cScatterResultMemInterimHost
    in
    let%bind type' = genType @@ Tuple type' in
    return @@ Cx.initStruct type' [ mapResultFinal; scatterResultFinal ]
  | ( Host
    , LoopKernel
        { kernel =
            { loopBlock =
                { frameShape = _
                ; mapArgs
                ; mapMemArgs
                ; mapIotas
                ; mapBody
                ; mapBodyMatcher
                ; mapResults
                ; mapResultMemFinal
                ; consumer =
                    Just
                      (ReducePar
                        { reduce = { arg; zero; body = reduceBody; d; type' = reduceType }
                        ; interimResultMemDeviceInterim = reduceResultsMemInterim
                        ; interimResultMemHostFinal = reduceResultsMemFinal
                        ; outerBody
                        })
                ; type'
                }
            ; mapResultMemDeviceInterim
            }
        ; captures
        ; blocks
        ; threads
        } ) ->
    let%bind capturePasses = handleCaptures captures in
    let%bind cMapResultMemInterimHost = genMem ~store:true mapResultMemDeviceInterim in
    let%bind cMapResultMemInterimType =
      genType ~wrapInPtr:true @@ Mem.type' mapResultMemDeviceInterim
    in
    let%bind cMapResultMemInterimDevice, mapResultMemInterimPass =
      createFunPass
        "mapResultMemInterim"
        cMapResultMemInterimType
        cMapResultMemInterimHost
    in
    let%bind cReduceResultsMemInterimHost = genMem ~store:true reduceResultsMemInterim in
    let%bind cReduceResultsMemInterimType =
      genType ~wrapInPtr:true @@ Mem.type' reduceResultsMemInterim
    in
    let%bind cReduceResultMemInterimDevice, reduceResultMemInterimPass =
      createFunPass
        "reduceResultsMemInterim"
        cReduceResultsMemInterimType
        cReduceResultsMemInterimHost
    in
    let%bind dHost = GenState.createVarAuto "d" @@ genDim d in
    let kernelPasses =
      capturePasses @ [ mapResultMemInterimPass; reduceResultMemInterimPass ]
    in
    let genComputeSum ~hostOrDevice ~op firstValue secondValue =
      let%bind () =
        GenState.write
        @@ C.Define
             { name = UniqueName arg.firstBinding; type' = None; value = Some firstValue }
      in
      let%bind () =
        GenState.write
        @@ C.Define
             { name = UniqueName arg.secondBinding
             ; type' = None
             ; value = Some secondValue
             }
      in
      genExpr ~hostOrDevice ~store:false op
    in
    (* Create the kernel *)
    let%bind kernelName =
      GenState.defineFun
        (NameOfStr { str = "reduceKernel"; needsUniquifying = true })
        ~f:(fun _ ->
          let%bind body =
            GenState.block
            @@
            let%bind cReduceType = genType reduceType in
            let%bind cache =
              GenState.createVar "cache"
              @@ fun name ->
              C.DefineDetail
                { attributes = [ "__shared__" ]
                ; name
                ; type' = Some cReduceType
                ; value = None
                ; dims = Cx.[ intLit threads; intLit Int.(threads + 1) ]
                }
            in
            let%bind dDevice = GenState.createVarAuto "d" @@ genDim d in
            let%bind blockRemainder =
              GenState.createVarAuto "blockRemainder" @@ Cx.(dDevice % intLit blocks)
            in
            let%bind elementsPerBlock =
              GenState.createVarAuto "elementsPerBlock" Cx.(dDevice / intLit blocks)
            in
            let%bind elementsThisBlock =
              GenState.createVarAuto "elementsThisBlock"
              @@ Cx.(
                   ternary
                     ~cond:(blockIndex < blockRemainder)
                     ~then':(elementsPerBlock + intLit 1)
                     ~else':elementsPerBlock)
            in
            let%bind blockStart =
              GenState.createVarAuto "blockStart"
              @@ Cx.(
                   ternary
                     ~cond:(blockIndex < blockRemainder)
                     ~then':(blockIndex * (elementsPerBlock + intLit 1))
                     ~else':((blockIndex * elementsPerBlock) + blockRemainder))
            in
            let%bind threadRemainder =
              GenState.createVarAuto "threadRemainder"
              @@ Cx.(elementsThisBlock % intLit threads)
            in
            let%bind elementsPerThread =
              GenState.createVarAuto "elementsPerThread"
              @@ Cx.(elementsThisBlock / intLit threads)
            in
            let%bind elementsThisThread =
              GenState.createVarAuto "elementsThisThread"
              @@ Cx.(
                   ternary
                     ~cond:(threadIndex < threadRemainder)
                     ~then':(elementsPerThread + intLit 1)
                     ~else':elementsPerThread)
            in
            let%bind threadSum =
              GenState.createVar "threadSum"
              @@ fun name -> C.Define { name; type' = Some cReduceType; value = None }
            in
            let%bind slices =
              GenState.createVarAuto "slices"
              @@ Cx.(
                   (elementsThisBlock + intLit Int.((threads * threads) - 1))
                   / intLit Int.(threads * threads))
            in
            (* loop over chunks to compute the thread sum *)
            let%bind () =
              GenState.writeForLoop
                ~loopVar:"slice"
                ~loopVarType:Int64
                ~initialValue:Cx.(intLit 0)
                ~cond:(fun slice -> Cx.(slice < slices))
                ~loopVarUpdate:IncrementOne
                ~body:(fun slice ->
                  let%bind () =
                    GenState.writeForLoop
                      ~loopVar:"i"
                      ~loopVarType:Int64
                      ~initialValue:Cx.(intLit 0)
                      ~cond:(fun i -> Cx.(i < intLit threads))
                      ~loopVarUpdate:IncrementOne
                      ~body:(fun i ->
                        let%bind offset =
                          GenState.createVarAuto "offset"
                          @@ Cx.(
                               ternary
                                 ~cond:(i < threadRemainder)
                                 ~then':(i * (elementsPerThread + intLit 1))
                                 ~else':((i * elementsPerThread) + threadRemainder))
                        in
                        let%bind indexInBlock =
                          GenState.createVarAuto "indexInBlock"
                          @@ Cx.(offset + (slice * intLit threads) + threadIndex)
                        in
                        GenState.writeIte
                          ~cond:Cx.(indexInBlock < elementsThisBlock)
                          ~thenBranch:
                            (let%bind index =
                               GenState.createVarAuto "index"
                               @@ Cx.(blockStart + indexInBlock)
                             in
                             let%bind () =
                               genMapBodyOnDevice
                                 ~loopVar:index
                                 ~loopSize:dDevice
                                 ~mapArgs
                                 ~mapIotas
                                 ~mapMemArgs
                                 ~mapBody
                                 ~mapBodyMatcher
                                 ~mapResults
                                 ~mapResultTypes:
                                   (match type' with
                                    | [ Tuple mapResultTypes; _ ] -> mapResultTypes
                                    | _ ->
                                      raise
                                      @@ Unreachable.Error
                                           "expected 2 element tuple where first element \
                                            is a tuple")
                                 ~mapResultMem:cMapResultMemInterimDevice
                             in
                             let%bind reduceArg =
                               genValueOfProductionTuple arg.production
                             in
                             GenState.write
                               Cx.(arrayDeref cache [ i; threadIndex ] := reduceArg))
                          ~elseBranch:(return ()))
                  in
                  let%bind () = GenState.write C.SyncThreads in
                  let%bind elementsThisThreadAndSlice =
                    GenState.createVarAuto "elementsThisThreadAndSlice"
                    @@ Cx.(
                         ternary
                           ~cond:(slice == slices - intLit 1)
                           ~then':(elementsThisThread % intLit threads)
                           ~else':(intLit threads))
                  in
                  let%bind () =
                    GenState.writeForLoop
                      ~loopVar:"i"
                      ~loopVarType:Int64
                      ~initialValue:Cx.(intLit 0)
                      ~cond:(fun i -> Cx.(i < elementsThisThreadAndSlice))
                      ~loopVarUpdate:IncrementOne
                      ~body:(fun i ->
                        GenState.writeIte
                          ~cond:Cx.(slice == intLit 0 && i == intLit 0)
                          ~thenBranch:
                            (GenState.write
                               Cx.(threadSum := arrayDeref cache [ threadIndex; i ]))
                          ~elseBranch:
                            (let%bind res =
                               genComputeSum
                                 ~hostOrDevice:Device
                                 ~op:reduceBody
                                 threadSum
                                 Cx.(arrayDeref cache [ threadIndex; i ])
                             in
                             GenState.write @@ Cx.(threadSum := res)))
                  in
                  let%bind () = GenState.write C.SyncThreads in
                  return ())
            in
            (* add up the thread sums and write them to the interim result *)
            let%bind () =
              GenState.write
              @@ Cx.(arrayDeref cache [ threadIndex; intLit 0 ] := threadSum)
            in
            let%bind () = GenState.write @@ C.SyncThreads in
            let%bind () =
              GenState.writeIte
                ~cond:Cx.(threadIndex == intLit 0 && elementsThisBlock > intLit 0)
                ~thenBranch:
                  (let%bind blockSum =
                     GenState.createVarAuto "blockSum"
                     @@ Cx.(arrayDeref cache [ intLit 0; intLit 0 ])
                   in
                   let%bind threadsWithSums =
                     GenState.createVarAuto "threadsWithSums"
                     @@ Cx.(
                          ternary
                            ~cond:(elementsPerThread == intLit 0)
                            ~then':threadRemainder
                            ~else':(intLit threads))
                   in
                   let%bind () =
                     GenState.writeForLoop
                       ~loopVar:"i"
                       ~loopVarType:Int64
                       ~initialValue:Cx.(intLit 1)
                       ~cond:(fun loopVar -> Cx.(loopVar < threadsWithSums))
                       ~loopVarUpdate:IncrementOne
                       ~body:(fun loopVar ->
                         let%bind res =
                           genComputeSum
                             ~hostOrDevice:Device
                             ~op:reduceBody
                             blockSum
                             Cx.(arrayDeref cache [ loopVar; intLit 0 ])
                         in
                         GenState.write @@ Cx.(blockSum := res))
                   in
                   let%bind cReduceResultMemInterimDeviceDerefer =
                     genArrayDeref
                       ~arrayType:(Type.array ~element:reduceType ~size:(Add d))
                       ~isMem:true
                       cReduceResultMemInterimDevice
                   in
                   let%bind () =
                     genCopyExprToMem
                       ~mem:(cReduceResultMemInterimDeviceDerefer blockIndex)
                       ~expr:blockSum
                       ~type':reduceType
                   in
                   return ())
                ~elseBranch:(return ())
            in
            return ()
          in
          return
          @@ C.
               { params = List.map kernelPasses ~f:(fun pass -> pass.param)
               ; body
               ; returnType = None
               ; funType = Kernel
               })
    in
    (* call the kernel *)
    let%bind () =
      GenState.write
      @@ C.Eval
           (KernelLaunch
              { kernel = kernelName
              ; blocks = Cx.intLit blocks
              ; threads = Cx.intLit threads
              ; args = List.map kernelPasses ~f:(fun pass -> pass.arg)
              })
    in
    (* copy the map result from device back to host *)
    let%bind mapResultFinal = genMem ~store:true mapResultMemFinal in
    let%bind () =
      genMoveMem
        `DeviceToHost
        ~type':(Mem.type' mapResultMemFinal)
        ~target:mapResultFinal
        ~source:cMapResultMemInterimHost
    in
    (* copy the reduce result from device back to host *)
    let%bind reduceResultsFinal = genMem ~store:true reduceResultsMemFinal in
    let%bind () =
      genMoveMem
        `DeviceToHost
        ~type':(Mem.type' reduceResultsMemFinal)
        ~target:reduceResultsFinal
        ~source:cReduceResultsMemInterimHost
    in
    (* Reduce the remaining elements *)
    let%bind reduceResultsMemFinalDerefer =
      genArrayDeref
        ~arrayType:(Mem.type' reduceResultsMemFinal)
        ~isMem:false
        reduceResultsFinal
    in
    let%bind reduceResultInitial = genExpr ~hostOrDevice:Host ~store:false zero in
    let%bind reduceResult = GenState.createVarAuto "reduceResult" reduceResultInitial in
    let%bind () =
      GenState.writeForLoop
        ~loopVar:"i"
        ~loopVarType:Int64
        ~initialValue:Cx.(intLit 0)
        ~cond:Cx.(fun i -> i < callBuiltin "std::min" [ intLit blocks; dHost ])
        ~loopVarUpdate:IncrementOne
        ~body:(fun i ->
          let%bind res =
            genComputeSum
              ~hostOrDevice:Host
              ~op:outerBody
              reduceResult
              (reduceResultsMemFinalDerefer i)
          in
          GenState.write @@ Cx.(reduceResult := res))
    in
    (* return *)
    let%bind type' = genType @@ Tuple type' in
    return @@ Cx.initStruct type' [ mapResultFinal; reduceResult ]
  | ( Host
    , LoopKernel
        { kernel =
            { loopBlock =
                { frameShape = _
                ; mapArgs
                ; mapMemArgs
                ; mapIotas
                ; mapBody
                ; mapBodyMatcher
                ; mapResults
                ; mapResultMemFinal
                ; consumer =
                    Just
                      (ScanPar
                        { scan =
                            { arg; zero; body; d; scanResultMemFinal; type' = scanType }
                        ; scanResultMemDeviceInterim
                        })
                ; type' = loopType
                }
            ; mapResultMemDeviceInterim
            }
        ; captures
        ; blocks
        ; threads
        } ) ->
    let genComputeSum firstValue secondValue =
      let%bind () =
        GenState.write
        @@ C.Define
             { name = UniqueName arg.firstBinding; type' = None; value = Some firstValue }
      in
      let%bind () =
        GenState.write
        @@ C.Define
             { name = UniqueName arg.secondBinding
             ; type' = None
             ; value = Some secondValue
             }
      in
      genExpr ~hostOrDevice:Device ~store:false body
    in
    let%bind cMapResultMemInterimHost = genMem ~store:true mapResultMemDeviceInterim in
    let%bind cMapResultMemInterimType =
      genType ~wrapInPtr:true @@ Mem.type' mapResultMemDeviceInterim
    in
    let%bind cMapResultMemInterimRef, mapResultMemInterimPass =
      createFunPass
        "mapResultMemInterim"
        cMapResultMemInterimType
        cMapResultMemInterimHost
    in
    let%bind cScanResultMemInterimHost = genMem ~store:true scanResultMemDeviceInterim in
    let%bind cZeroType = genType @@ Expr.type' zero in
    let%bind cZeroHost = genExpr ~hostOrDevice:Host ~store:true zero in
    let%bind capturePasses = handleCaptures captures in
    (* runScan creates a `scanRunner` function and calls it. It also generates
       kernels that `scanRunner` calls. runScan can create either a version with or without
       maps. "with maps" means that it will perform the mapping when it reads its input.
       This is necessary bedcause the with map version relies on the no map version. *)
    let rec runScan
      ~input:mapOrCInputFromCall
      ~output:cOutputFromCall
      ~outputType
      ~n:cNFromCall
      ~zero:cZeroFromCall
      =
      let scanElementType = Expr.type' body in
      let%bind cScanElementType = genType scanElementType in
      let%bind cOutputType = genType outputType in
      let%bind cOutput, outputPass = createFunPass "output" cOutputType cOutputFromCall in
      let%bind n, nPass = createFunPass "n" C.Int64 cNFromCall in
      let%bind cZeroRef, zeroPass = createFunPass "init" cZeroType cZeroFromCall in
      let%bind mapOrCInput, passes, includeMaps =
        match mapOrCInputFromCall with
        | `WithMap ->
          return
            ( `WithMap
            , outputPass :: mapResultMemInterimPass :: nPass :: zeroPass :: capturePasses
            , true )
        | `NoMap cInputFromCall ->
          let cInputType = cOutputType in
          let%bind cInput, inputPass = createFunPass "input" cInputType cInputFromCall in
          return
            ( `NoMap cInput
            , inputPass :: outputPass :: nPass :: zeroPass :: capturePasses
            , false )
      in
      let runnerName = if includeMaps then "scanRunnerWithMap" else "scanRunnerNoMap" in
      let%bind scanRunner =
        GenState.defineFun
          (NameOfStr { str = runnerName; needsUniquifying = true })
          ~f:(fun scanRunner ->
            let%bind runnerBody =
              GenState.block
              @@
              let maxKernelProcessingSize = 2 * blocks * threads in
              let conflictFreeOffset n =
                let logNumBanks = 4 in
                Int.shift_right n logNumBanks
              in
              let conflictFreeOffsetC n =
                let logNumBanks = 4 in
                Cx.(n >> intLit logNumBanks)
              in
              let%bind () = GenState.comment "how many full runs we can do" in
              let%bind runs =
                GenState.createVarAuto "runs" @@ Cx.(n / intLit maxKernelProcessingSize)
              in
              let%bind () = GenState.comment "how many elements don't fit in full runs" in
              let%bind unevenSize =
                GenState.createVarAuto "unevenSize"
                @@ Cx.(n % intLit maxKernelProcessingSize)
              in
              let%bind () = GenState.comment "number of full blocks that fit in uneven" in
              let%bind unevenBlocks =
                GenState.createVarAuto "unevenBlocks"
                @@ Cx.(unevenSize / intLit Int.(2 * threads))
              in
              let%bind () =
                GenState.comment "number of elements that don't fit in one block"
              in
              let%bind lastBlockSize =
                GenState.createVarAuto "lastBlockSize"
                @@ Cx.(unevenSize % intLit Int.(2 * threads))
              in
              (* hacky workaround for blockCount here. It needs to be used to define the
                 size of an array malloced. To avoid substantial code changes, we create
                 an id that we bind the blockCount to and then treat that id as something
                 available in the index environment *)
              let%bind blockCountId = GenState.createId "blockCount" in
              let%bind () =
                GenState.write
                @@ C.Define
                     { name = UniqueName blockCountId
                     ; type' = None
                     ; value =
                         Some
                           Cx.(
                             (n + intLit Int.((2 * threads) - 1))
                             / intLit Int.(2 * threads))
                     }
              in
              let blockCount = Cx.refId blockCountId in
              let sumsType =
                Type.array
                  ~element:scanElementType
                  ~size:(Add (Index.dimensionRef blockCountId))
              in
              let%bind sums =
                genMalloc ~hostOrDevice:Host ~memLoc:MallocDevice ~store:true sumsType
              in
              let%bind sumsAsMemDerefer =
                genArrayDeref ~arrayType:sumsType ~isMem:true sums
              in
              let sumsSummedType =
                Type.array
                  ~element:scanElementType
                  ~size:
                    (Add
                       (Index.addDimensions
                          (Index.dimensionConstant 1)
                          (Index.dimensionRef blockCountId)))
              in
              let%bind sumsSummed =
                genMalloc
                  ~hostOrDevice:Host
                  ~memLoc:MallocDevice
                  ~store:true
                  sumsSummedType
              in
              let%bind () = GenState.comment "do scan on chunks" in
              let innerCapturePasses =
                List.map capturePasses ~f:(fun { arg = _; param } ->
                  { arg = VarRef param.name; param })
              in
              let makeParam str type' =
                let%bind name =
                  GenState.createName @@ NameOfStr { str; needsUniquifying = true }
                in
                return (C.VarRef name, ({ name; type' } : C.funParam))
              in
              let getInputElementOrDoMap
                ~dDevice
                ~mapResultMemInterim
                ~inputDerefer
                ~loopVar
                =
                if includeMaps
                then (
                  let%bind () =
                    genMapBodyOnDevice
                      ~loopVar
                      ~loopSize:dDevice
                      ~mapArgs
                      ~mapIotas
                      ~mapMemArgs
                      ~mapBody
                      ~mapBodyMatcher
                      ~mapResults
                      ~mapResultTypes:
                        (match loopType with
                         | [ Tuple mapResultTypes; _ ] -> mapResultTypes
                         | _ ->
                           raise
                           @@ Unreachable.Error
                                "expected 2 element tuple where first element is a tuple")
                      ~mapResultMem:mapResultMemInterim
                  in
                  genValueOfProductionTuple arg.production)
                else return @@ inputDerefer loopVar
              in
              let%bind scanKernel =
                GenState.defineFun
                  (NameOfStr
                     { str =
                         (if includeMaps then "scanKernelWithMaps" else "scanKernelNoMaps")
                     ; needsUniquifying = true
                     })
                  ~f:(fun _ ->
                    let%bind input, inputParam = makeParam "input" cOutputType in
                    let%bind mapResultMemInterimType =
                      genType ~wrapInPtr:true @@ Mem.type' mapResultMemDeviceInterim
                    in
                    let%bind mapResultMemInterim, mapResultMemInterimParam =
                      makeParam "mapResultMemInterim" mapResultMemInterimType
                    in
                    let%bind output, outputParam = makeParam "output" cOutputType in
                    let%bind chunkOffset, chunkOffsetParam =
                      makeParam "chunkOffset" Int64
                    in
                    let%bind totalSums, totalSumsParam =
                      makeParam "totalSums" cOutputType
                    in
                    let%bind scanKernelBody =
                      GenState.block
                      @@
                      let%bind temp =
                        GenState.createVar "temp"
                        @@ fun name ->
                        C.DefineDetail
                          { attributes = [ "__shared__" ]
                          ; name
                          ; type' = Some cScanElementType
                          ; value = None
                          ; dims =
                              [ Cx.intLit ((threads * 2) + conflictFreeOffset threads) ]
                          }
                      in
                      let%bind offset = GenState.createVarAuto "offset" @@ Cx.intLit 1 in
                      let%bind ai = GenState.createVarAuto "ai" @@ threadIndex in
                      let%bind bi =
                        GenState.createVarAuto "bi" @@ Cx.(threadIndex + intLit threads)
                      in
                      let%bind bankOffsetA =
                        GenState.createVarAuto "bankOffsetA" @@ conflictFreeOffsetC ai
                      in
                      let%bind bankOffsetB =
                        GenState.createVarAuto "bankOffsetB" @@ conflictFreeOffsetC bi
                      in
                      let%bind dDevice = GenState.createVarAuto "d" @@ genDim d in
                      let%bind inputDerefer =
                        genArrayDeref ~arrayType:scanType ~isMem:false input
                      in
                      (* loadInputAndDoMap does
                         temp[i + bankOffset] = f(input[blockIdx.x * n + i])
                         f is a no-op if includesMap is false *)
                      let loadInputAndDoMap i bankOffset =
                        (* wrap in a block in order to reduce the scope of the map
                           productions, since those variables are re-used for both the a
                           and b instances *)
                        let%bind block =
                          GenState.block
                          @@
                          let%bind scanArg =
                            getInputElementOrDoMap
                              ~dDevice
                              ~inputDerefer
                              ~mapResultMemInterim
                              ~loopVar:
                                Cx.(
                                  chunkOffset
                                  + (blockIndex * intLit threads * intLit 2)
                                  + i)
                          in
                          GenState.write
                            Cx.(arrayDeref temp [ i + bankOffset ] := scanArg)
                        in
                        GenState.write @@ C.Block block
                      in
                      let%bind () = loadInputAndDoMap ai bankOffsetA in
                      let%bind () = loadInputAndDoMap bi bankOffsetB in
                      let%bind () =
                        GenState.writeForLoop
                          ~loopVar:"d"
                          ~loopVarType:Int64
                          ~initialValue:(Cx.intLit threads)
                          ~cond:(fun d -> Cx.(d > intLit 0))
                          ~loopVarUpdate:(ShiftRight (Cx.intLit 1))
                          ~body:(fun d ->
                            let%bind () = GenState.write @@ C.SyncThreads in
                            let%bind () =
                              GenState.writeIte
                                ~cond:Cx.(threadIndex < d)
                                ~thenBranch:
                                  (let%bind ai =
                                     GenState.createVarAuto
                                       "ai"
                                       Cx.(
                                         (offset * ((intLit 2 * threadIndex) + intLit 1))
                                         - intLit 1)
                                   in
                                   let%bind bi =
                                     GenState.createVarAuto
                                       "bi"
                                       Cx.(
                                         (offset * ((intLit 2 * threadIndex) + intLit 2))
                                         - intLit 1)
                                   in
                                   let%bind () =
                                     GenState.write
                                     @@ Cx.(ai := ai + conflictFreeOffsetC ai)
                                   in
                                   let%bind () =
                                     GenState.write
                                     @@ Cx.(bi := bi + conflictFreeOffsetC bi)
                                   in
                                   let%bind sum =
                                     genComputeSum
                                       Cx.(arrayDeref temp [ ai ])
                                       Cx.(arrayDeref temp [ bi ])
                                   in
                                   GenState.write Cx.(arrayDeref temp [ bi ] := sum))
                                ~elseBranch:(return ())
                            in
                            let%bind () =
                              GenState.write @@ Cx.(offset := offset * intLit 2)
                            in
                            return ())
                      in
                      let%bind () =
                        GenState.writeForLoop
                          ~loopVar:"d"
                          ~loopVarType:Int64
                          ~initialValue:(Cx.intLit 1)
                          ~cond:(fun d -> Cx.(d < intLit threads * intLit 2))
                          ~loopVarUpdate:(ShiftLeft (Cx.intLit 1))
                          ~body:(fun d ->
                            let%bind () =
                              GenState.write Cx.(offset := offset >> intLit 1)
                            in
                            let%bind () = GenState.write @@ C.SyncThreads in
                            let%bind () =
                              GenState.writeIte
                                ~cond:Cx.(threadIndex < d)
                                ~thenBranch:
                                  (let%bind ai =
                                     GenState.createVarAuto
                                       "ai"
                                       Cx.(
                                         (offset * ((intLit 2 * threadIndex) + intLit 1))
                                         - intLit 1)
                                   in
                                   let%bind bi =
                                     GenState.createVarAuto
                                       "bi"
                                       Cx.(
                                         (offset * ((intLit 2 * threadIndex) + intLit 2))
                                         - intLit 1)
                                   in
                                   let%bind () =
                                     GenState.write
                                     @@ Cx.(ai := ai + conflictFreeOffsetC ai)
                                   in
                                   let%bind () =
                                     GenState.write
                                     @@ Cx.(bi := bi + conflictFreeOffsetC bi)
                                   in
                                   GenState.writeIte
                                     ~cond:Cx.(threadIndex == intLit 0)
                                     ~thenBranch:
                                       (GenState.write
                                          Cx.(
                                            arrayDeref temp [ bi ]
                                              := arrayDeref temp [ ai ]))
                                     ~elseBranch:
                                       (let%bind t =
                                          GenState.createVarAuto
                                            "t"
                                            Cx.(arrayDeref temp [ ai ])
                                        in
                                        let%bind () =
                                          GenState.write
                                            Cx.(
                                              arrayDeref temp [ ai ]
                                                := arrayDeref temp [ bi ])
                                        in
                                        let%bind sum =
                                          genComputeSum t Cx.(arrayDeref temp [ bi ])
                                        in
                                        GenState.write Cx.(arrayDeref temp [ bi ] := sum)))
                                ~elseBranch:(return ())
                            in
                            return ())
                      in
                      let%bind () = GenState.write C.SyncThreads in
                      let%bind outputAsMemDerefer =
                        genArrayDeref ~arrayType:outputType ~isMem:true output
                      in
                      let%bind () =
                        genCopyExprToMem
                          ~mem:
                            (outputAsMemDerefer
                               Cx.(
                                 chunkOffset
                                 + (blockIndex * intLit threads * intLit 2)
                                 + ai))
                          ~expr:Cx.(arrayDeref temp [ ai + bankOffsetA ])
                          ~type':scanElementType
                      in
                      let%bind () =
                        genCopyExprToMem
                          ~mem:
                            (outputAsMemDerefer
                               Cx.(
                                 chunkOffset
                                 + (blockIndex * intLit threads * intLit 2)
                                 + bi))
                          ~expr:Cx.(arrayDeref temp [ bi + bankOffsetB ])
                          ~type':scanElementType
                      in
                      let%bind () = GenState.write @@ C.SyncThreads in
                      let%bind () =
                        GenState.writeIte
                          ~cond:Cx.(threadIndex == intLit 0)
                          ~thenBranch:
                            (let%bind outputDerefer =
                               genArrayDeref ~arrayType:outputType ~isMem:false output
                             in
                             let a =
                               outputDerefer
                                 Cx.(
                                   chunkOffset
                                   + (blockIndex * intLit threads * intLit 2)
                                   + ((intLit threads * intLit 2) - intLit 1))
                             in
                             let%bind b =
                               getInputElementOrDoMap
                                 ~dDevice
                                 ~inputDerefer
                                 ~mapResultMemInterim
                                 ~loopVar:
                                   Cx.(
                                     chunkOffset
                                     + (blockIndex * intLit threads * intLit 2)
                                     + (intLit threads * intLit 2)
                                     - intLit 1)
                             in
                             let%bind sum = genComputeSum a b in
                             (* sumsType is not technically the correct array type
                                because the length is wrong, but that doesn't matter
                                for genArrayDeref *)
                             let%bind totalSumsDerefer =
                               genArrayDeref ~arrayType:sumsType ~isMem:true totalSums
                             in
                             genCopyExprToMem
                               ~mem:(totalSumsDerefer blockIndex)
                               ~expr:sum
                               ~type':scanElementType)
                          ~elseBranch:(return ())
                      in
                      return ()
                    in
                    let commonParams : C.funParam list =
                      outputParam
                      :: chunkOffsetParam
                      :: totalSumsParam
                      :: List.map innerCapturePasses ~f:(fun p -> p.param)
                    in
                    let%bind params =
                      if includeMaps
                      then return (mapResultMemInterimParam :: commonParams)
                      else return (inputParam :: commonParams)
                    in
                    return
                    @@ C.
                         { params
                         ; body = scanKernelBody
                         ; returnType = None
                         ; funType = Kernel
                         })
              in
              let%bind () =
                GenState.writeForLoop
                  ~loopVar:"i"
                  ~loopVarType:Int64
                  ~initialValue:(Cx.intLit 0)
                  ~cond:(fun i -> Cx.(i < runs))
                  ~loopVarUpdate:IncrementOne
                  ~body:(fun i ->
                    let commonArgs =
                      cOutput
                      :: Cx.(intLit maxKernelProcessingSize * i)
                      :: sumsAsMemDerefer Cx.(i * intLit blocks)
                      :: List.map innerCapturePasses ~f:(fun p -> p.arg)
                    in
                    let args =
                      match mapOrCInput with
                      | `WithMap -> cMapResultMemInterimRef :: commonArgs
                      | `NoMap cInput -> cInput :: commonArgs
                    in
                    GenState.write
                    @@ C.Eval
                         (KernelLaunch
                            { kernel = scanKernel
                            ; blocks = Cx.intLit blocks
                            ; threads = Cx.intLit threads
                            ; args
                            }))
              in
              let%bind () =
                GenState.writeIte
                  ~cond:Cx.(unevenBlocks != intLit 0)
                  ~thenBranch:
                    (let commonArgs =
                       cOutput
                       :: Cx.(intLit maxKernelProcessingSize * runs)
                       :: sumsAsMemDerefer Cx.(runs * intLit blocks)
                       :: List.map innerCapturePasses ~f:(fun p -> p.arg)
                     in
                     let args =
                       match mapOrCInput with
                       | `WithMap -> cMapResultMemInterimRef :: commonArgs
                       | `NoMap cInput -> cInput :: commonArgs
                     in
                     GenState.write
                     @@ C.Eval
                          (KernelLaunch
                             { kernel = scanKernel
                             ; blocks = unevenBlocks
                             ; threads = Cx.intLit threads
                             ; args
                             }))
                  ~elseBranch:(return ())
              in
              let%bind naiveScanKernel =
                GenState.defineFun
                  (NameOfStr
                     { str =
                         (if includeMaps
                          then "naiveScanKernelWithMaps"
                          else "naiveScanKernelNoMaps")
                     ; needsUniquifying = true
                     })
                  ~f:(fun _ ->
                    let%bind input, inputParam = makeParam "input" cOutputType in
                    let%bind mapResultMemInterimType =
                      genType ~wrapInPtr:true @@ Mem.type' mapResultMemDeviceInterim
                    in
                    let%bind mapResultMemInterim, mapResultMemInterimParam =
                      makeParam "mapResultMemInterim" mapResultMemInterimType
                    in
                    let%bind output, outputParam = makeParam "output" cOutputType in
                    let%bind chunkOffset, chunkOffsetParam =
                      makeParam "chunkOffset" Int64
                    in
                    let%bind n, nParam = makeParam "n" Int64 in
                    let%bind totalSums, totalSumsParam =
                      makeParam "totalSums" cOutputType
                    in
                    let%bind naiveScanKernelBody =
                      GenState.block
                      @@
                      let%bind temp =
                        GenState.createVar "temp"
                        @@ fun name ->
                        C.DefineDetail
                          { attributes = [ "__shared__" ]
                          ; name
                          ; type' = Some cScanElementType
                          ; value = None
                          ; dims = [ Cx.intLit (threads * 10) ]
                          }
                      in
                      let%bind pout = GenState.createVarAuto "pout" @@ Cx.intLit 0 in
                      let%bind () =
                        GenState.comment
                          "This is exclusive scan, so shift right by one and set first \
                           element to 0"
                      in
                      let%bind dDevice = GenState.createVarAuto "d" @@ genDim d in
                      let%bind inputDerefer =
                        genArrayDeref ~arrayType:scanType ~isMem:false input
                      in
                      let%bind () =
                        GenState.writeIte
                          ~cond:Cx.(threadIndex > intLit 0)
                          ~thenBranch:
                            (let%bind value =
                               getInputElementOrDoMap
                                 ~dDevice
                                 ~inputDerefer
                                 ~mapResultMemInterim
                                 ~loopVar:Cx.(chunkOffset + threadIndex - intLit 1)
                             in
                             GenState.write
                             @@ Cx.(arrayDeref temp [ (pout * n) + threadIndex ] := value))
                          ~elseBranch:(return ())
                      in
                      let%bind () = GenState.write C.SyncThreads in
                      let%bind () =
                        GenState.writeForLoop
                          ~loopVar:"offset"
                          ~loopVarType:Int64
                          ~initialValue:Cx.(intLit 1)
                          ~cond:(fun offset -> Cx.(offset < n))
                          ~loopVarUpdate:(ShiftLeft Cx.(intLit 1))
                          ~body:(fun offset ->
                            let%bind () = GenState.write Cx.(pout := intLit 1 - pout) in
                            let%bind pin =
                              GenState.createVarAuto "pint" Cx.(intLit 1 - pout)
                            in
                            let%bind () =
                              GenState.writeIte
                                ~cond:Cx.(threadIndex > offset)
                                ~thenBranch:
                                  (let%bind sum =
                                     genComputeSum
                                       Cx.(
                                         arrayDeref
                                           temp
                                           [ (pin * n) + threadIndex - offset ])
                                       Cx.(arrayDeref temp [ (pin * n) + threadIndex ])
                                   in
                                   GenState.write
                                     Cx.(
                                       arrayDeref temp [ (pout * n) + threadIndex ] := sum))
                                ~elseBranch:
                                  (GenState.write
                                     Cx.(
                                       arrayDeref temp [ (pout * n) + threadIndex ]
                                       := arrayDeref temp [ (pin * n) + threadIndex ]))
                            in
                            GenState.write C.SyncThreads)
                      in
                      let%bind outputDerefer =
                        genArrayDeref ~arrayType:outputType ~isMem:true output
                      in
                      let%bind () =
                        genCopyExprToMem
                          ~mem:(outputDerefer Cx.(chunkOffset + threadIndex))
                          ~expr:Cx.(arrayDeref temp [ (pout * n) + threadIndex ])
                          ~type':scanElementType
                      in
                      let%bind () = GenState.write C.SyncThreads in
                      GenState.writeIte
                        ~cond:Cx.(threadIndex == n - intLit 1)
                        ~thenBranch:
                          (let%bind scanArg =
                             getInputElementOrDoMap
                               ~dDevice
                               ~inputDerefer
                               ~mapResultMemInterim
                               ~loopVar:Cx.(chunkOffset + n - intLit 1)
                           in
                           let%bind sum =
                             genComputeSum
                               Cx.(arrayDeref temp [ (pout * n) + threadIndex ])
                               scanArg
                           in
                           (* sumsType is not technically the correct array type
                              because the length is wrong, but that doesn't matter
                              for genArrayDeref *)
                           let%bind totalSumsDerefer =
                             genArrayDeref ~arrayType:sumsType ~isMem:true totalSums
                           in
                           genCopyExprToMem
                             ~mem:(totalSumsDerefer blockIndex)
                             ~expr:sum
                             ~type':scanElementType)
                        ~elseBranch:(return ())
                    in
                    let commonParams : C.funParam list =
                      outputParam
                      :: chunkOffsetParam
                      :: nParam
                      :: totalSumsParam
                      :: List.map innerCapturePasses ~f:(fun p -> p.param)
                    in
                    let%bind params =
                      if includeMaps
                      then return (mapResultMemInterimParam :: commonParams)
                      else return (inputParam :: commonParams)
                    in
                    return
                    @@ C.
                         { params
                         ; body = naiveScanKernelBody
                         ; returnType = None
                         ; funType = Kernel
                         })
              in
              let%bind () =
                GenState.writeIte
                  ~cond:Cx.(lastBlockSize != intLit 0)
                  ~thenBranch:
                    (let%bind lastOffset =
                       GenState.createVarAuto
                         "lastOffset"
                         Cx.(
                           (intLit maxKernelProcessingSize * runs)
                           + (unevenBlocks * intLit threads * intLit 2))
                     in
                     let commonArgs =
                       cOutput
                       :: lastOffset
                       :: lastBlockSize
                       :: sumsAsMemDerefer Cx.((runs * intLit blocks) + unevenBlocks)
                       :: List.map innerCapturePasses ~f:(fun p -> p.arg)
                     in
                     let args =
                       match mapOrCInput with
                       | `WithMap -> cMapResultMemInterimRef :: commonArgs
                       | `NoMap cInput -> cInput :: commonArgs
                     in
                     GenState.write
                     @@ C.Eval
                          (KernelLaunch
                             { kernel = naiveScanKernel
                             ; blocks = Cx.intLit 1
                             ; threads = lastBlockSize
                             ; args
                             }))
                  ~elseBranch:(return ())
              in
              let%bind () =
                GenState.writeIte
                  ~cond:Cx.(runs != intLit 0 || unevenBlocks != intLit 0)
                  ~thenBranch:
                    (if includeMaps
                     then
                       runScan
                         ~input:(`NoMap sums)
                         ~output:sumsSummed
                         ~outputType:sumsSummedType
                         ~n:blockCount
                         ~zero:cZeroRef
                     else (
                       let args =
                         sums
                         :: sumsSummed
                         :: n
                         :: cZeroRef
                         :: List.map capturePasses ~f:(fun p -> C.VarRef p.param.name)
                       in
                       GenState.write @@ C.Eval Cx.(call scanRunner args)))
                  ~elseBranch:
                    (let%bind sumsSummedDerefer =
                       genArrayDeref ~arrayType:sumsSummedType ~isMem:true sumsSummed
                     in
                     let%bind _ =
                       genCopyExprBetweenMachines
                         ~copyDir:`HostToDevice
                         ~type':(Expr.type' zero)
                         ~targetMem:(sumsSummedDerefer @@ Cx.intLit 0)
                         cZeroRef
                     in
                     return ())
              in
              let%bind () =
                GenState.writeIte
                  ~cond:Cx.(lastBlockSize != intLit 0)
                  ~thenBranch:
                    (let%bind outputDerefer =
                       genArrayDeref ~arrayType:outputType ~isMem:true cOutput
                     in
                     genMoveMem
                       `DeviceToDevice
                       ~type':scanElementType
                       ~target:(outputDerefer n)
                       ~source:
                         (sumsAsMemDerefer Cx.((runs * intLit blocks) + unevenBlocks)))
                  ~elseBranch:(return ())
              in
              let%bind adjBlockValues, adjBlockValuesPass =
                createFunPass "values" cOutputType cOutput
              in
              let%bind cSumsSummedType = genType ~wrapInPtr:true sumsSummedType in
              let%bind adjBlockAdjs, adjBlockAdjsPass =
                createFunPass "adjs" cSumsSummedType sumsSummed
              in
              let%bind adjBlockN, adjBlockNPass =
                createFunPass "n" Int64 Cx.(n + intLit 1)
              in
              let adjBlockPasses =
                adjBlockValuesPass :: adjBlockAdjsPass :: adjBlockNPass :: capturePasses
              in
              let adjBlockName =
                if includeMaps then "adjBlockWithMap" else "adjBlockNoMap"
              in
              let%bind adjBlock =
                GenState.defineFun
                  (NameOfStr { str = adjBlockName; needsUniquifying = true })
                  ~f:(fun _ ->
                    let%bind adjBlockBody =
                      GenState.block
                      @@
                      let%bind valOffset =
                        GenState.createVarAuto
                          "valOffset"
                          Cx.(
                            (refStr "blockDim" %. StrName "x" * blockIndex) + threadIndex)
                      in
                      let%bind adjOffset =
                        GenState.createVarAuto "adjOffset" blockIndex
                      in
                      GenState.writeIte
                        ~cond:Cx.(valOffset < adjBlockN)
                        ~thenBranch:
                          (let%bind valuesAsMemDerefer =
                             genArrayDeref
                               ~arrayType:outputType
                               ~isMem:true
                               adjBlockValues
                           in
                           let%bind adjsDerefer =
                             genArrayDeref
                               ~arrayType:sumsSummedType
                               ~isMem:false
                               adjBlockAdjs
                           in
                           GenState.writeIte
                             ~cond:Cx.(threadIndex == intLit 0)
                             ~thenBranch:
                               (genCopyExprToMem
                                  ~mem:(valuesAsMemDerefer valOffset)
                                  ~expr:(adjsDerefer adjOffset)
                                  ~type':scanElementType)
                             ~elseBranch:
                               (let%bind valuesDerefer =
                                  genArrayDeref
                                    ~arrayType:outputType
                                    ~isMem:false
                                    adjBlockValues
                                in
                                let%bind sum =
                                  genComputeSum
                                    (adjsDerefer adjOffset)
                                    (valuesDerefer valOffset)
                                in
                                genCopyExprToMem
                                  ~mem:(valuesAsMemDerefer valOffset)
                                  ~expr:sum
                                  ~type':scanElementType))
                        ~elseBranch:(return ())
                    in
                    return
                    @@ C.
                         { params = List.map adjBlockPasses ~f:(fun p -> p.param)
                         ; body = adjBlockBody
                         ; returnType = None
                         ; funType = Kernel
                         })
              in
              GenState.write
              @@ C.Eval
                   (C.KernelLaunch
                      { kernel = adjBlock
                      ; blocks = Cx.(blockCount + intLit 1)
                      ; threads = Cx.(intLit Int.(threads * 2))
                      ; args = List.map adjBlockPasses ~f:(fun p -> p.arg)
                      })
            in
            return
            @@ C.
                 { params = List.map passes ~f:(fun p -> p.param)
                 ; body = runnerBody
                 ; returnType = None
                 ; funType = Host
                 })
      in
      GenState.write @@ Cx.(eval (call scanRunner (List.map passes ~f:(fun p -> p.arg))))
    in
    let%bind () =
      runScan
        ~input:`WithMap
        ~output:cScanResultMemInterimHost
        ~outputType:scanType
        ~n:(genDim d)
        ~zero:cZeroHost
    in
    (* copy scan result from device to host *)
    let%bind scanResultFinal = genMem ~store:true scanResultMemFinal in
    let%bind () =
      genMoveMem
        `DeviceToHost
        ~type':(Mem.type' scanResultMemFinal)
        ~target:scanResultFinal
        ~source:cScanResultMemInterimHost
    in
    (* copy map result from device to host *)
    let%bind mapResultFinal = genMem ~store:true mapResultMemFinal in
    let%bind () =
      genMoveMem
        `DeviceToHost
        ~type':(Mem.type' mapResultMemFinal)
        ~target:mapResultFinal
        ~source:cMapResultMemInterimHost
    in
    (* return *)
    let%bind type' = genType @@ Tuple loopType in
    return @@ Cx.initStruct type' [ mapResultFinal; scanResultFinal ]
  | ( _
    , ContiguousSubArray
        { arrayArg; indexArg; originalShape = _; resultShape = _; type' = outType } ) ->
    let%bind cArrayArg = genExpr ~hostOrDevice ~store:true arrayArg in
    let%bind cIndexArg = genExpr ~hostOrDevice ~store:true indexArg in
    let indexArgLen =
      match Expr.type' indexArg with
      | Array { element = Literal IntLiteral; shape = [ Add l ] } -> l
      | _ -> raise @@ Unreachable.Error "expected a 1-d array of integers"
    in
    let%bind cIndexArgLen = GenState.createVarAuto "indexArgLen" @@ genDim indexArgLen in
    let rec genSubArray (cArrayArg : C.expr) (inType : Type.t) (outType : Type.t) =
      match inType with
      | Array { element = _; shape } ->
        let shape = NeList.to_list shape in
        (* shape and indexArg need to be iterated together, but shape is a list of shape
           elements. While individual elements are reified, the whole thing is not. This
           is the reason for the below odd iteration structure *)
        let%bind offset = GenState.createVarAuto "offset" @@ Cx.intLit 0 in
        (* indexIndex is the index in indexArg that we are currently on *)
        let%bind indexIndex = GenState.createVarAuto "indexIndex" @@ Cx.intLit 0 in
        let%bind () =
          GenState.writeForLoop
            ~loopVar:"elementIndex"
            ~loopVarType:Int64
            ~initialValue:(Cx.intLit 0)
            ~cond:(fun elementIndex -> Cx.(elementIndex < intLit (List.length shape)))
            ~loopVarUpdate:IncrementOne
            ~body:(fun elementIndex ->
              List.foldi shape ~init:(return ()) ~f:(fun i acc shapeElement ->
                GenState.writeIte
                  ~cond:Cx.(intLit i == elementIndex)
                  ~thenBranch:
                    (let writeUpdate currentDimSize =
                       let%bind () =
                         GenState.write @@ Cx.(offset := offset * currentDimSize)
                       in
                       let%bind () =
                         GenState.writeIte
                           ~cond:Cx.(indexIndex < cIndexArgLen)
                           ~thenBranch:
                             (let%bind () =
                                GenState.write
                                  Cx.(
                                    offset := offset + arrayDeref cIndexArg [ indexIndex ])
                              in
                              let%bind () =
                                GenState.write Cx.(indexIndex := indexIndex + intLit 1)
                              in
                              return ())
                           ~elseBranch:(return ())
                       in
                       return ()
                     in
                     match shapeElement with
                     | Add d -> writeUpdate (genDim d)
                     | ShapeRef shapeRef ->
                       let dims = Cx.(refId shapeRef %. sliceDimsFieldName) in
                       let dimCount = Cx.(refId shapeRef %. sliceDimCountFieldName) in
                       let%bind () =
                         GenState.writeForLoop
                           ~loopVar:"i"
                           ~loopVarType:Int64
                           ~initialValue:(Cx.intLit 0)
                           ~cond:(fun i -> Cx.(i < dimCount))
                           ~loopVarUpdate:IncrementOne
                           ~body:(fun i -> writeUpdate Cx.(arrayDeref dims [ i ]))
                       in
                       return ())
                  ~elseBranch:acc))
        in
        (match outType with
         | Atom _ -> return Cx.(arrayDeref cArrayArg [ offset ])
         | Array _ -> return Cx.(cArrayArg + offset)
         | Tuple _ -> raise Unreachable.default)
      | Tuple inElements ->
        let outElements =
          match outType with
          | Tuple outElements -> outElements
          | _ -> raise Unreachable.default
        in
        let%bind type' = genType ~wrapInPtr:true outType in
        let%bind elements =
          List.zip_exn inElements outElements
          |> List.mapi ~f:(fun i (elementType, outElementType) ->
            genSubArray Cx.(cArrayArg %. tupleFieldName i) elementType outElementType)
          |> GenState.all
        in
        return @@ Cx.initStruct type' elements
      | Atom _ -> (* This case should only be hit when indexArg is [] *) return cArrayArg
    in
    let%bind subarray = genSubArray cArrayArg (Expr.type' arrayArg) outType in
    storeIfRequested ~name:"subarray" subarray
  | Host, IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' } ->
    let rec genParallelism (parallelism : Expr.parallelism) =
      match parallelism with
      | KnownParallelism n -> Cx.(intLit n)
      | Parallelism { shape; rest } ->
        let shapeSize = genShapeElementSize shape in
        Cx.(shapeSize * genParallelism rest)
      | MaxParallelism pars ->
        Cx.callBuiltin "std::max" [ Arr (List.map pars ~f:genParallelism) ]
    in
    let parallelism = genParallelism parallelism in
    let%bind cType = genType type' in
    let%bind resVar =
      GenState.createVar "parResult"
      @@ fun name -> C.Define { name; type' = Some cType; value = None }
    in
    let writeBranch branch =
      let%bind res = genExpr ~hostOrDevice ~store:false branch in
      GenState.write @@ Cx.(resVar := res)
    in
    let%bind () =
      GenState.writeIte
        ~cond:Cx.(parallelism >= intLit cutoff)
        ~thenBranch:(writeBranch then')
        ~elseBranch:(writeBranch else')
    in
    return resVar
  | _, Eseq { statement; expr; type' = _ } ->
    let%bind () = genStmnt ~hostOrDevice statement in
    genExpr ~hostOrDevice ~store expr
  | _, Getmem { addr; type' } ->
    let rec genGetmem (mem : C.expr) (type' : Type.t) =
      match type' with
      | Array _ -> return mem
      | Tuple elements ->
        let%bind elements =
          elements
          |> List.mapi ~f:(fun i element ->
            genGetmem Cx.(mem %. tupleFieldName i) element)
          |> GenState.all
        in
        let%bind destType = genType ~wrapInPtr:false type' in
        return @@ Cx.initStruct destType elements
      | Atom (Sigma _)
      | Atom (Literal (IntLiteral | FloatLiteral | BooleanLiteral | CharacterLiteral)) ->
        return @@ C.PtrDeref mem
    in
    let%bind mem = genMem ~store:true addr in
    genGetmem mem type'

and genLet
  : type l sIn sOut.
    hostOrDevice:l hostOrDevice
    -> genBody:(sIn -> (sOut, _) GenState.u)
    -> (l, sIn, Expr.captures) Expr.let'
    -> (sOut, _) GenState.u
  =
  fun ~hostOrDevice ~genBody Expr.{ args; body } ->
  let open GenState.Let_syntax in
  let%bind () =
    args
    |> List.map ~f:(fun { binding; value } ->
      let%bind varType = genType @@ Expr.type' value in
      let%bind value = genExpr ~hostOrDevice ~store:false value in
      GenState.write
      @@ C.Define { name = UniqueName binding; type' = Some varType; value = Some value })
    |> GenState.all_unit
  in
  genBody body

and genMallocLet
  : type l sIn sOut.
    hostOrDevice:l hostOrDevice
    -> genBody:(sIn -> (sOut, _) GenState.u)
    -> sIn Expr.mallocLet
    -> (sOut, _) GenState.u
  =
  fun ~hostOrDevice ~genBody { memArgs; body } ->
  let open GenState.Let_syntax in
  let%bind () =
    memArgs
    |> List.map ~f:(fun { memBinding; memType; memLoc } ->
      let%bind varType = genType ~wrapInPtr:true memType in
      let%bind mem = genMalloc ~hostOrDevice ~store:false ~memLoc memType in
      GenState.write
      @@ C.Define { name = UniqueName memBinding; type' = Some varType; value = Some mem })
    |> GenState.all_unit
  in
  genBody body

and genMapBodySetup
  ~(loopVar : C.expr)
  ~(loopSize : C.expr)
  ~(mapArgs : Expr.mapArg list)
  ~(mapIotas : Expr.mapIota list)
  ~(mapMemArgs : Expr.memArg list)
  : (unit, _) GenState.u
  =
  let open GenState.Let_syntax in
  let%bind () =
    mapArgs
    |> List.map ~f:(fun { binding; ref } ->
      let%bind derefer =
        genArrayDeref ~arrayType:ref.type' ~isMem:false @@ Cx.refId ref.id
      in
      GenState.write
      @@ C.Define
           { name = UniqueName binding; type' = None; value = Some (derefer loopVar) })
    |> GenState.all_unit
  in
  let%bind () =
    mapMemArgs
    |> List.map ~f:(fun { memBinding; mem } ->
      let%bind cMem = genMem ~store:true mem in
      let%bind derefer = genArrayDeref ~arrayType:(Mem.type' mem) ~isMem:true @@ cMem in
      GenState.write
      @@ C.Define
           { name = UniqueName memBinding; type' = None; value = Some (derefer loopVar) })
    |> GenState.all_unit
  in
  let%bind () =
    mapIotas |> List.map ~f:(genIota ~loopVar ~loopSize) |> GenState.all_unit
  in
  return ()

and genMapBodyOnDevice
  ~(loopVar : C.expr)
  ~(loopSize : C.expr)
  ~(mapArgs : Expr.mapArg list)
  ~(mapIotas : Expr.mapIota list)
  ~(mapMemArgs : Expr.memArg list)
  ~(mapBody : device Expr.withCaptures)
  ~(mapBodyMatcher : Expr.tupleMatch)
  ~(mapResults : Identifier.t list)
  ~(mapResultTypes : Type.tuple)
  ~(mapResultMem : C.expr)
  : (unit, _) GenState.u
  =
  let open GenState.Let_syntax in
  (* perform the map body *)
  let%bind () = genMapBodySetup ~loopVar ~loopSize ~mapArgs ~mapIotas ~mapMemArgs in
  let%bind mapResult = genExpr ~hostOrDevice:Device ~store:true mapBody in
  let%bind () = genMatchMapBody mapBodyMatcher mapResult in
  (* write the results of the map *)
  let%bind mapResultDerefersAndTypes =
    mapResultTypes
    |> List.mapi ~f:(fun i resultTypeArr ->
      let%bind derefer =
        genArrayDeref
          ~arrayType:resultTypeArr
          ~isMem:true
          Cx.(mapResultMem %. tupleFieldName i)
      in
      return @@ (derefer, guillotineType resultTypeArr))
    |> GenState.all
  in
  let%bind () =
    List.zip_exn mapResults mapResultDerefersAndTypes
    |> List.map ~f:(fun (resultId, (resultDerefer, resultType)) ->
      genCopyExprToMem
        ~expr:Cx.(refId resultId)
        ~mem:(resultDerefer loopVar)
        ~type':resultType)
    |> GenState.all_unit
  in
  return ()
;;

let genPrint type' value =
  let open GenState.Let_syntax in
  match type' with
  | Type.Atom (Literal (IntLiteral | FloatLiteral | CharacterLiteral | BooleanLiteral)) ->
    GenState.write @@ C.Eval Cx.(refStr "std::cout" << value << charLit '\n')
  | Type.Atom (Sigma _) -> raise Unimplemented.default
  | Type.Array { element; shape } ->
    let%bind cElement = genType (Atom element) in
    let shape = NeList.to_list shape in
    let%bind dimCount = GenState.createVarAuto "dimCount" @@ genShapeDimCount shape in
    let%bind dims =
      GenState.createVarAuto "dims"
      @@ Cx.(callBuiltin "mallocHost" ~typeArgs:[ Int64 ] [ dimCount ])
    in
    let%bind _ = reifyShapeIndexToMem ~mem:dims shape in
    let%bind () =
      GenState.write
      @@ C.Eval
           Cx.(callBuiltin "printArray" ~typeArgs:[ cElement ] [ value; dims; dimCount ])
    in
    return ()
  | Type.Tuple _ -> raise Unimplemented.default
;;

let genMainBlock (deviceInfo : DeviceInfo.t) (main : withCaptures) =
  let open GenState.Let_syntax in
  GenState.block
  @@
  let%bind () =
    (* reserve heap space *)
    GenState.write
    @@ C.Eval
         Cx.(
           callBuiltin
             "HANDLE_ERROR"
             [ callBuiltin
                 "cudaDeviceSetLimit"
                 [ refStr "cudaLimitMallocHeapSize"
                 ; intLit deviceInfo.globalMemoryBytes / intLit 10
                 ]
             ])
  in
  let%bind cVal = genExpr ~hostOrDevice:Host ~store:true main in
  let%bind () = genPrint (Expr.type' main) cVal in
  return ()
;;

let codegen (deviceInfo : DeviceInfo.t) (prog : withCaptures)
  : (CompilerState.state, C.t, _) State.t
  =
  let builder =
    let open CBuilder.Let_syntax in
    let%map _, block = GenState.run (genMainBlock deviceInfo prog) GenState.emptyState in
    Some block
  in
  CBuilder.build ~prelude builder
;;

module Stage (SB : Source.BuilderT) = struct
  type state = CompilerState.state
  type input = withCaptures
  type output = C.t
  type error = (SB.source option, string) Source.annotate

  let name = "Code Generation"

  let run input =
    CompilerPipeline.S.make ~f:(fun (inState : state) ->
      State.run (codegen inState.deviceInfo input) inState)
  ;;
end
