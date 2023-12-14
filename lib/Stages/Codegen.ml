open! Base
open Acorn

let prelude =
  {|#include <cstdio>
#include <algorithm>

static void HandleError(cudaError_t err,
                        const char *file,
                        int line) {
  if (err != cudaSuccess) {
    printf("%s in %s at line %d\n", cudaGetErrorString(err),
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
T *mallocDevice(int64_t count) {
  T* array;
  HANDLE_ERROR(cudaMalloc((void **) &array, count * sizeof(T)));
  return array;
};

template<typename T>
T* copyHostToDevice(T* hostArray, int64_t count) {
  T* deviceArray = mallocDevice<T>(count);
  HANDLE_ERROR(cudaMemcpy(deviceArray, hostArray, count * sizeof(T), cudaMemcpyHostToDevice));
  return deviceArray;
};

template<typename T>
T* copyDeviceToHost(T* deviceArray, int64_t count) {
  T* hostArray = mallocHost<T>(count);
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

  type state =
    { statementsRev : C.statement list
    ; typeCache : C.type' Map.M(Type).t
    ; ptrWrappedTypeCache : C.type' Map.M(Type).t
    }

  let emptyState =
    { statementsRev = []
    ; typeCache = Map.empty (module Type)
    ; ptrWrappedTypeCache = Map.empty (module Type)
    }
  ;;

  type ('a, 'e) u = (state, 'a, 'e) t

  (* let defineFun (name : CBuilder.name) ~(f : C.name -> (C.fun', 'e) u) : (C.name, 'e) u =
     makeF ~f:(fun inState ->
     CBuilder.defineFunL name ~f:(fun name -> run (f name) inState))
     ;; *)

  let defineStruct (name : CBuilder.name) ~(f : C.name -> (C.struct', 'e) u)
    : (C.name, 'e) u
    =
    makeF ~f:(fun inState ->
      CBuilder.defineStructL name ~f:(fun name -> run (f name) inState))
  ;;

  let writeStatement statement =
    let open Let_syntax in
    let%bind state = get () in
    let%map () = set { state with statementsRev = statement :: state.statementsRev } in
    ()
  ;;

  let flushBlock () =
    let open Let_syntax in
    let%bind state = get () in
    let block = List.rev state.statementsRev in
    let%map () = set { state with statementsRev = [] } in
    block
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

  let storeExpr ?(name = "storedExpr") (expr : C.expr) =
    let open Let_syntax in
    match expr with
    | Literal _ | VarRef _ -> (* Don't need to store if its a constant *) return expr
    | _ ->
      let%bind name = createName (NameOfStr { str = name; needsUniquifying = true }) in
      let%map () = writeStatement @@ C.Define { name; type' = None; value = Some expr } in
      C.VarRef name
  ;;
end

let tupleFieldName i = C.Name.StrName [%string "_%{i#Int}"]
let boxValueFieldName = C.Name.StrName "value"
let sliceDimCountFieldName = C.Name.StrName "dimCount"
let sliceDimsFieldName = C.Name.StrName "dims"

let genTypeForSort sort =
  match sort with
  | Sort.Dim -> C.Int64
  | Sort.Shape -> C.TypeRef (StrName "Slice")
;;

let rec genType ?(wrapInPtr = false) type' : (C.type', _) GenState.u =
  let open GenState.Let_syntax in
  let%bind (state : GenState.state) = GenState.get () in
  let typeCache = if wrapInPtr then state.ptrWrappedTypeCache else state.typeCache in
  match Map.find typeCache type' with
  | Some cType -> return cType
  | None ->
    let%bind cType =
      match type' with
      | Type.Atom (Literal IntLiteral) ->
        return @@ if wrapInPtr then C.Ptr C.Int64 else C.Int64
      | Type.Atom (Literal CharacterLiteral) ->
        return @@ if wrapInPtr then C.Ptr C.Char else C.Char
      | Type.Atom (Literal BooleanLiteral) ->
        return @@ if wrapInPtr then C.Ptr C.Bool else C.Bool
      | Type.Array array ->
        let elementType = array.element in
        let%map elementCType = genType ~wrapInPtr:false (Atom elementType) in
        C.Ptr elementCType
      | Type.Atom (Sigma { parameters; body }) ->
        let%map name =
          GenState.defineStruct
            (NameOfStr { str = "Box"; needsUniquifying = true })
            ~f:(fun _ ->
              let paramFields =
                List.map parameters ~f:(fun { binding; bound } ->
                  C.{ name = UniqueName binding; type' = genTypeForSort bound })
              in
              let%map valueType = genType ~wrapInPtr:false body in
              let valueField = C.{ name = boxValueFieldName; type' = valueType } in
              valueField :: paramFields)
        in
        if wrapInPtr then C.Ptr (C.TypeRef name) else C.TypeRef name
      | Type.Tuple elements ->
        let%bind elementTypes =
          elements |> List.map ~f:(genType ~wrapInPtr) |> GenState.all
        in
        let%map name =
          GenState.defineStruct
            (NameOfStr { str = "Tuple"; needsUniquifying = true })
            ~f:(fun _ ->
              return
              @@ List.mapi elementTypes ~f:(fun i type' ->
                C.{ name = tupleFieldName i; type' }))
        in
        C.TypeRef name
    in
    let%map () =
      GenState.modify ~f:(fun (state : GenState.state) ->
        if wrapInPtr
        then
          { state with
            ptrWrappedTypeCache = Map.set state.typeCache ~key:type' ~data:cType
          }
        else { state with typeCache = Map.set state.typeCache ~key:type' ~data:cType })
    in
    cType
;;

type 'l hostOrDevice = Host : host hostOrDevice
(* | Device : device hostOrDevice *)

let genDim ({ const; refs; lens } : Index.dimension) =
  let addRefs init =
    Map.fold refs ~init ~f:(fun ~key:dimVar ~data:multiplier cValSoFar ->
      C.Binop
        { op = "+"
        ; arg1 = cValSoFar
        ; arg2 =
            C.Binop
              { op = "*"
              ; arg1 = C.VarRef (UniqueName dimVar)
              ; arg2 = C.Literal (Int64Literal multiplier)
              }
        })
  in
  let addLens init =
    Map.fold lens ~init ~f:(fun ~key:sliceVar ~data:multiplier cValSoFar ->
      C.Binop
        { op = "+"
        ; arg1 = cValSoFar
        ; arg2 =
            C.Binop
              { op = "*"
              ; arg1 =
                  C.FieldDeref
                    { value = VarRef (UniqueName sliceVar)
                    ; fieldName = sliceDimCountFieldName
                    }
              ; arg2 = C.Literal (Int64Literal multiplier)
              }
        })
  in
  addLens @@ addRefs @@ C.Literal (Int64Literal const)
;;

let genShapeElementSize = function
  | Index.Add dim -> genDim dim
  | Index.ShapeRef shapeRef ->
    C.FunCall
      { fun' = StrName "product"
      ; typeArgs = None
      ; args = [ VarRef (UniqueName shapeRef) ]
      }
;;

let genShapeSize shape =
  List.fold shape ~init:(C.Literal (Int64Literal 1)) ~f:(fun acc se ->
    C.Binop { op = "*"; arg1 = acc; arg2 = genShapeElementSize se })
;;

let genShapeDimCount shape =
  let knownDimCount, unknownDimCounts =
    List.fold shape ~init:(0, []) ~f:(fun (knownSoFar, unknownSoFar) shapeElement ->
      match shapeElement with
      | Index.Add _ -> knownSoFar + 1, unknownSoFar
      | Index.ShapeRef ref ->
        let refCount =
          C.FieldDeref
            { value = VarRef (UniqueName ref); fieldName = sliceDimCountFieldName }
        in
        knownSoFar, refCount :: unknownSoFar)
  in
  List.fold
    unknownDimCounts
    ~init:(C.Literal (Int64Literal knownDimCount))
    ~f:(fun acc dimCount -> C.Binop { op = "+"; arg1 = acc; arg2 = dimCount })
;;

let reifyShapeIndexToMem ~mem shape =
  let open GenState.Let_syntax in
  let sumOffsets intOffset sliceSizeOffsets =
    List.fold
      sliceSizeOffsets
      ~init:(C.Literal (Int64Literal intOffset))
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
          let%map () =
            GenState.writeStatement
            @@ C.Assign
                 { lhs = C.ArrayDeref { value = mem; index = offset }; rhs = genDim dim }
          in
          intOffset + 1, sliceSizeOffsets
        | Index.ShapeRef id ->
          let slice = C.VarRef (UniqueName id) in
          let sliceSize =
            C.FieldDeref { value = slice; fieldName = sliceDimCountFieldName }
          in
          let%map () =
            GenState.writeStatement
            @@ C.Eval
                 (FunCall
                    { fun' = StrName "copySameLoc"
                    ; typeArgs = Some [ Int64 ]
                    ; args =
                        [ Binop { op = "+"; arg1 = mem; arg2 = offset }
                        ; C.FieldDeref { value = slice; fieldName = sliceDimsFieldName }
                        ; sliceSize
                        ]
                    })
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
    let stridedIndex index = C.Binop { op = "*"; arg1 = index; arg2 = stride } in
    return
    @@
    if resultIsPointer
    then fun index -> C.Binop { op = "+"; arg1 = array; arg2 = stridedIndex index }
    else fun index -> C.ArrayDeref { value = array; index = stridedIndex index }
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
        genArrayDeref ~arrayType:arrayElemType ~resultType:resultElemType ~isMem
        @@ C.FieldDeref { value = array; fieldName = tupleFieldName i })
      |> GenState.all
    in
    return
    @@ fun index ->
    C.StructConstructor
      { type'
      ; args = List.map elementBuilders ~f:(fun elementBuilder -> elementBuilder index)
      }
  | Type.Atom _ -> raise @@ Unreachable.Error "Expected array type"
;;

let rec genMem
  : type l. hostOrDevice:l hostOrDevice -> store:bool -> l Mem.t -> (C.expr, _) GenState.u
  =
  fun ~hostOrDevice ~store expr ->
  let open GenState.Let_syntax in
  let storeIfRequested ~name e = if store then GenState.storeExpr ~name e else return e in
  match hostOrDevice, expr with
  | _, Ref { id; type' = _ } -> return @@ C.VarRef (UniqueName id)
  | Host, Malloc { hostOrDevice; type' } ->
    let mallocer =
      match hostOrDevice with
      | MallocHost -> "mallocHost"
      | MallocDevice -> "mallocHost"
    in
    let rec mallocType type' =
      match type' with
      | Type.Array { element; shape } ->
        let%bind cElementType = genType ~wrapInPtr:false (Atom element) in
        let numElements = genShapeSize @@ NeList.to_list shape in
        return
        @@ C.FunCall
             { fun' = StrName mallocer
             ; typeArgs = Some [ cElementType ]
             ; args = [ numElements ]
             }
      | Type.Tuple elements ->
        let%bind cType = genType ~wrapInPtr:true (Tuple elements) in
        let%bind mallocedElements = elements |> List.map ~f:mallocType |> GenState.all in
        return @@ C.StructConstructor { type' = cType; args = mallocedElements }
      | Type.Atom _ ->
        let%bind cType = genType ~wrapInPtr:false type' in
        return
        @@ C.FunCall
             { fun' = StrName mallocer
             ; typeArgs = Some [ cType ]
             ; args = [ Literal (Int64Literal 1) ]
             }
    in
    mallocType type'
  | _, Values { elements; type' } ->
    let%bind elements =
      elements |> List.map ~f:(genMem ~hostOrDevice ~store) |> GenState.all
    in
    let%bind type' = genType ~wrapInPtr:true type' in
    storeIfRequested ~name:"memValues" @@ C.StructConstructor { type'; args = elements }
  | _, TupleDeref { tuple; index; type' = _ } ->
    let%map tuple = genMem ~hostOrDevice ~store tuple in
    C.FieldDeref { value = tuple; fieldName = tupleFieldName index }
  | _, Index { mem; offset; type' } ->
    let memType = Mem.type' mem in
    let%bind offset = genDim offset |> GenState.storeExpr ~name:"offset" in
    let%bind mem = genMem ~hostOrDevice ~store:true mem in
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
      let%bind loopVar =
        GenState.createName (NameOfStr { str = "i"; needsUniquifying = true })
      in
      let%bind body =
        GenState.block
        @@ genCopyExprToMem
             ~memNeedsPtrDeref:false
             ~mem:(ArrayDeref { value = mem; index = VarRef loopVar })
             ~expr:(ArrayDeref { value = expr; index = VarRef loopVar })
             ~type':(Atom elementType)
      in
      GenState.writeStatement
      @@ C.ForLoop
           { loopVar
           ; loopVarType = Int64
           ; initialValue = Literal (Int64Literal 0)
           ; cond = Binop { op = "<"; arg1 = VarRef loopVar; arg2 = size }
           ; loopVarUpdate = IncrementOne
           ; body
           }
    | Tuple elements ->
      elements
      |> List.mapi ~f:(fun i element ->
        genCopyExprToMem
          ~memNeedsPtrDeref
          ~mem:(C.FieldDeref { value = mem; fieldName = tupleFieldName i })
          ~expr:(C.FieldDeref { value = expr; fieldName = tupleFieldName i })
          ~type':element)
      |> GenState.all_unit
    | Atom (Sigma { parameters; body }) ->
      let%bind () =
        parameters
        |> List.map ~f:(fun param ->
          let%bind () =
            GenState.writeStatement
            @@ C.Assign
                 { lhs =
                     C.fieldDeref
                       ~value:mem
                       ~fieldName:(UniqueName param.binding)
                       ~valueIsPtr:memNeedsPtrDeref
                 ; rhs = FieldDeref { value = expr; fieldName = UniqueName param.binding }
                 }
          in
          GenState.writeStatement
          @@ C.Define
               { name = UniqueName param.binding
               ; type' = Some (genTypeForSort param.bound)
               ; value =
                   Some
                     (FieldDeref { value = expr; fieldName = UniqueName param.binding })
               })
        |> GenState.all_unit
      in
      genCopyExprToMem
        ~memNeedsPtrDeref:true
        ~mem:
          (C.fieldDeref
             ~value:mem
             ~fieldName:boxValueFieldName
             ~valueIsPtr:memNeedsPtrDeref)
        ~expr:(FieldDeref { value = expr; fieldName = boxValueFieldName })
        ~type':body
    | Atom (Literal IntLiteral)
    | Atom (Literal BooleanLiteral)
    | Atom (Literal CharacterLiteral) ->
      let mem = if memNeedsPtrDeref then C.PtrDeref mem else mem in
      GenState.writeStatement @@ C.Assign { lhs = mem; rhs = expr }
  in
  genCopyExprToMem ~memNeedsPtrDeref:true
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
    let%bind addr = genMem ~hostOrDevice ~store:true addr in
    let%bind expr = genExpr ~hostOrDevice ~store:true expr in
    genCopyExprToMem ~expr ~mem:addr ~type'
  | Host, MapKernel _ -> raise Unimplemented.default
  | _, ComputeForSideEffects expr ->
    let%bind expr = genExpr ~hostOrDevice ~store:false expr in
    GenState.writeStatement @@ C.Eval expr
  | _, SLet let' -> genLet ~hostOrDevice ~genBody:(genStmnt ~hostOrDevice) let'
  | _, SMemLet memLet -> genMemLet ~hostOrDevice ~genBody:(genStmnt ~hostOrDevice) memLet
  | _, ReifyShapeIndex { shape; mem } ->
    let%bind mem = genMem ~hostOrDevice ~store:true mem in
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
    let indicesAndParams = List.zip_exn indices type'.parameters in
    let%bind indices =
      indicesAndParams
      |> List.map ~f:(fun (index, param) ->
        let%bind type' = genType (Expr.type' index.expr) in
        let%bind indexExpr = genExpr ~hostOrDevice ~store:false index.expr in
        let index =
          match index.index with
          | Shape shape ->
            C.StructConstructor { type'; args = [ indexExpr; genShapeDimCount shape ] }
          | Dimension _ -> indexExpr
        in
        let paramName = C.Name.UniqueName param.binding in
        let%bind () =
          GenState.writeStatement
          @@ C.Define { name = paramName; type' = Some type'; value = Some index }
        in
        return @@ C.VarRef paramName)
      |> GenState.all
    in
    let%bind type' = genType (Atom (Sigma type')) in
    let%bind body = genExpr ~hostOrDevice ~store:false body in
    return @@ C.StructConstructor { type'; args = body :: indices }
  | _, Literal (IntLiteral i) -> return @@ C.Literal (Int64Literal i)
  | _, Literal (CharacterLiteral c) -> return @@ C.Literal (CharLiteral c)
  | _, Literal (BooleanLiteral b) -> return @@ C.Literal (BoolLiteral b)
  | _, ScalarPrimitive { op; args; type' = _ } ->
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
    (match op with
     | Add -> genBinop "+"
     | Sub -> genBinop "-"
     | Mul -> genBinop "*"
     | Div -> genBinop "/"
     | Equal -> genBinop "==")
  | _, TupleDeref { tuple; index; type' = _ } ->
    let%map tuple = genExpr ~hostOrDevice ~store tuple in
    C.FieldDeref { value = tuple; fieldName = tupleFieldName index }
  | _, Values { elements; type' } ->
    let%bind elements =
      elements |> List.map ~f:(genExpr ~hostOrDevice ~store:false) |> GenState.all
    in
    let%bind type' = genType (Tuple type') in
    storeIfRequested ~name:"values" @@ C.StructConstructor { type'; args = elements }
  | _, Ref { id; type' = _ } -> return @@ C.VarRef (UniqueName id)
  | _, BoxValue { box; type' = _ } ->
    let%map box = genExpr ~hostOrDevice ~store box in
    C.FieldDeref { value = box; fieldName = boxValueFieldName }
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
               C.StructConstructor
                 { type' = genTypeForSort Shape; args = [ cv; dimCount ] })
          | FromBox { box; i } ->
            let params =
              match Expr.type' box with
              | Atom (Sigma sigma) -> sigma.parameters
              | _ -> raise @@ Unreachable.Error "expected sigma type"
            in
            let param = List.nth_exn params i in
            let%map box = genExpr ~hostOrDevice ~store:false box in
            C.FieldDeref { value = box; fieldName = UniqueName param.binding }
        in
        let type' =
          match sort with
          | Dim -> C.Int64
          | Shape -> C.Ptr Int64
        in
        GenState.writeStatement
        @@ C.Define
             { name = UniqueName indexBinding
             ; type' = Some type'
             ; value = Some indexValue
             })
      |> GenState.all_unit
    in
    genExpr ~hostOrDevice ~store body
  | _, Let let' -> genLet ~hostOrDevice ~genBody:(genExpr ~hostOrDevice ~store) let'
  | _, MemLet memLet ->
    genMemLet ~hostOrDevice ~genBody:(genExpr ~hostOrDevice ~store) memLet
  | _, ReifyDimensionIndex { dim } -> GenState.storeExpr ~name:"reifiedDim" @@ genDim dim
  | ( _
    , LoopBlock
        { frameShape
        ; mapArgs
        ; mapMemArgs
        ; mapIotas
        ; mapBody
        ; mapBodyMatcher
        ; mapResults
        ; mapResultMem
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
        let%bind argRef = genMem ~hostOrDevice ~store:true arg.mem in
        let%bind derefer =
          genArrayDeref ~arrayType:(Mem.type' arg.mem) ~isMem:true argRef
        in
        return @@ (arg, derefer))
      |> GenState.all
    in
    let%bind cMapResultMem = genMem ~hostOrDevice ~store:true mapResultMem in
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
            (C.FieldDeref { value = cMapResultMem; fieldName = tupleFieldName i })
        in
        return @@ (derefer, guillotineType resultTypeArr))
      |> GenState.all
    in
    let%bind loopVar =
      GenState.createName (NameOfStr { str = "i"; needsUniquifying = true })
    in
    let%bind consumerInLoop, consumerResult =
      match consumer with
      | None ->
        let%bind unitType = genType @@ Tuple [] in
        return @@ (return (), C.StructConstructor { type' = unitType; args = [] })
      | Some (ReduceSeq { arg; zero; mappedMemArgs; body; d = _; character; type' = _ })
        ->
        let accVar = C.Name.UniqueName arg.firstBinding in
        let stepVar = C.Name.UniqueName arg.secondBinding in
        let%bind accType = genType @@ Expr.type' body in
        let%bind cZero =
          zero |> Option.map ~f:(genExpr ~hostOrDevice ~store:false) |> GenState.all_opt
        in
        let%bind () =
          GenState.writeStatement
          @@ C.Define { name = accVar; type' = Some accType; value = cZero }
        in
        let%bind reduceMemArgsWithDerefers =
          mappedMemArgs
          |> List.map ~f:(fun arg ->
            let%bind argRef = genMem ~hostOrDevice ~store:true arg.mem in
            let%bind derefer =
              genArrayDeref ~arrayType:(Mem.type' arg.mem) ~isMem:true argRef
            in
            return @@ (arg, derefer))
          |> GenState.all
        in
        let%bind characterInLoop, characterResult =
          match character with
          | Reduce -> return @@ (return (), C.VarRef accVar)
          | Scan mem ->
            let%bind cMem = genMem ~hostOrDevice ~store:true mem in
            let%bind memDerefer =
              genArrayDeref ~arrayType:(Mem.type' mem) ~isMem:true cMem
            in
            let inLoop =
              genCopyExprToMem
                ~mem:(memDerefer (VarRef loopVar))
                ~expr:(VarRef accVar)
                ~type':(Expr.type' body)
            in
            return (inLoop, cMem)
          | OpenScan mem ->
            (* Ignoring the case of open scan with no zero since it is non-sensical *)
            let%bind cMem = genMem ~hostOrDevice ~store:true mem in
            let%bind memDerefer =
              genArrayDeref ~arrayType:(Mem.type' mem) ~isMem:true cMem
            in
            let%bind () =
              genCopyExprToMem
                ~mem:(memDerefer (Literal (Int64Literal 0)))
                ~expr:(VarRef accVar)
                ~type':(Expr.type' body)
            in
            let inLoop =
              genCopyExprToMem
                ~mem:
                  (memDerefer
                   @@ Binop
                        { op = "+"
                        ; arg1 = VarRef loopVar
                        ; arg2 = Literal (Int64Literal 1)
                        })
                ~expr:(VarRef accVar)
                ~type':(Expr.type' body)
            in
            return (inLoop, cMem)
        in
        let inLoop =
          let rec getValueOfProductionTuple
            : Expr.productionTuple -> (C.expr, _) GenState.u
            = function
            | ProductionTuple { elements; type' } ->
              let elementType = guillotineType type' in
              let%bind cType = genType elementType in
              let%bind args =
                elements |> List.map ~f:getValueOfProductionTuple |> GenState.all
              in
              return @@ C.StructConstructor { type' = cType; args }
            | ProductionTupleAtom production ->
              return @@ C.VarRef (UniqueName production.productionId)
          in
          let%bind stepValue = getValueOfProductionTuple arg.production in
          let%bind () =
            GenState.writeStatement
            @@ C.Define { name = stepVar; type' = Some accType; value = Some stepValue }
          in
          let%bind () =
            reduceMemArgsWithDerefers
            |> List.map ~f:(fun (arg, derefer) ->
              GenState.writeStatement
              @@ C.Define
                   { name = UniqueName arg.memBinding
                   ; type' = None
                   ; value = Some (derefer (VarRef loopVar))
                   })
            |> GenState.all_unit
          in
          let doReduceStep =
            let%bind body = genExpr ~hostOrDevice ~store:false body in
            GenState.writeStatement @@ C.Assign { lhs = VarRef accVar; rhs = body }
          in
          match zero with
          | Some _ ->
            (* If there is an initial zero, perform the reduce step every iteration *)
            doReduceStep
          | None ->
            (* If there is no initial zero, on the first iteration,
               just assign the step var to the acc var *)
            let%bind firstIteration =
              GenState.block
              @@ GenState.writeStatement
                   (C.Assign { lhs = VarRef accVar; rhs = VarRef stepVar })
            in
            let%bind subsequentIteration = GenState.block @@ doReduceStep in
            let%bind () =
              GenState.writeStatement
              @@ C.Ite
                   { cond =
                       C.Binop
                         { op = "=="
                         ; arg1 = VarRef loopVar
                         ; arg2 = Literal (Int64Literal 0)
                         }
                   ; thenBranch = firstIteration
                   ; elseBranch = subsequentIteration
                   }
            in
            characterInLoop
        in
        return @@ (inLoop, characterResult)
      | Some (Fold _) -> raise Unimplemented.default
      | Some (Scatter _) -> raise Unimplemented.default
    in
    let%bind body =
      GenState.block
      @@
      let%bind () =
        mapArgsWithDerefers
        |> List.map ~f:(fun (arg, derefer) ->
          GenState.writeStatement
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
          GenState.writeStatement
          @@ C.Define
               { name = UniqueName arg.memBinding
               ; type' = None
               ; value = Some (derefer (VarRef loopVar))
               })
        |> GenState.all_unit
      in
      let%bind () =
        mapIotas
        |> List.map ~f:(fun { iota; nestIn } ->
          let value =
            match nestIn with
            | None -> C.VarRef loopVar
            | Some parentIota ->
              C.Binop
                { op = "+"
                ; arg1 =
                    Binop
                      { op = "*"; arg1 = VarRef (UniqueName parentIota); arg2 = steps }
                ; arg2 = VarRef loopVar
                }
          in
          GenState.writeStatement
          @@ C.Define { name = UniqueName iota; type' = Some Int64; value = Some value })
        |> GenState.all_unit
      in
      let%bind mapRes = genExpr ~hostOrDevice ~store:true mapBody in
      let rec matchBody (matcher : Expr.tupleMatch) res =
        match matcher with
        | Binding id ->
          GenState.writeStatement
          @@ C.Define { name = UniqueName id; type' = None; value = Some res }
        | Unpack matchers ->
          matchers
          |> List.mapi ~f:(fun i matcher ->
            matchBody matcher
            @@ C.FieldDeref { value = res; fieldName = tupleFieldName i })
          |> GenState.all_unit
      in
      let%bind () = matchBody mapBodyMatcher mapRes in
      let%bind () =
        List.zip_exn mapResults mapResultDerefersAndTypes
        |> List.map ~f:(fun (resultId, (resultDerefer, resultType)) ->
          genCopyExprToMem
            ~expr:(C.VarRef (UniqueName resultId))
            ~mem:(resultDerefer @@ VarRef loopVar)
            ~type':resultType)
        |> GenState.all_unit
      in
      consumerInLoop
    in
    let%bind () =
      GenState.writeStatement
      @@ C.ForLoop
           { loopVar
           ; loopVarType = Int64
           ; initialValue = Literal (Int64Literal 0)
           ; cond = Binop { op = "<"; arg1 = VarRef loopVar; arg2 = steps }
           ; loopVarUpdate = IncrementOne
           ; body
           }
    in
    let%bind mapResult =
      genExpr ~hostOrDevice ~store
      @@ Getmem { addr = mapResultMem; type' = Mem.type' mapResultMem }
    in
    let%bind type' = genType @@ Tuple type' in
    return @@ C.StructConstructor { type'; args = [ mapResult; consumerResult ] }
  | Host, LoopKernel _ -> raise Unimplemented.default
  | _, SubArray { arrayArg; indexArg = _; type' } ->
    let%bind array = genExpr ~hostOrDevice ~store:true arrayArg in
    let rec genSubArray (array : C.expr) (type' : Type.t) =
      match type' with
      | Array _ -> raise Unimplemented.default
      | Tuple elements ->
        let%bind type' = genType ~wrapInPtr:true type' in
        let%bind elements =
          elements
          |> List.mapi ~f:(fun i elementType ->
            genSubArray
              (C.FieldDeref { value = array; fieldName = tupleFieldName i })
              elementType)
          |> GenState.all
        in
        return @@ C.StructConstructor { type'; args = elements }
      | Atom _ -> (* This case should only be hit when indexArg is [] *) return array
    in
    genSubArray array type'
  | Host, IfParallelismHitsCutoff { parallelism; cutoff; then'; else'; type' = _ } ->
    let rec genParallelism (parallelism : Expr.parallelism) =
      match parallelism with
      | KnownParallelism n -> C.Literal (Int64Literal n)
      | Parallelism { shape; rest } ->
        let shapeSize = genShapeElementSize shape in
        C.Binop { op = "*"; arg1 = shapeSize; arg2 = genParallelism rest }
      | MaxParallelism pars ->
        C.FunCall
          { fun' = StrName "std::max"
          ; typeArgs = None
          ; args = [ Arr (List.map pars ~f:genParallelism) ]
          }
    in
    let parallelism = genParallelism parallelism in
    let%bind resVar =
      GenState.createName (NameOfStr { str = "parResult"; needsUniquifying = true })
    in
    let writeBranch branch =
      let%bind res = genExpr ~hostOrDevice ~store:false branch in
      let%bind () =
        GenState.writeStatement @@ C.Assign { lhs = VarRef resVar; rhs = res }
      in
      GenState.flushBlock ()
    in
    let%bind thenBranch = writeBranch then' in
    let%bind elseBranch = writeBranch else' in
    let%bind () =
      GenState.writeStatement
      @@ C.Ite
           { cond =
               Binop
                 { op = ">="; arg1 = parallelism; arg2 = Literal (Int64Literal cutoff) }
           ; thenBranch
           ; elseBranch
           }
    in
    return @@ C.VarRef resVar
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
            genGetmem (C.FieldDeref { value = mem; fieldName = tupleFieldName i }) element)
          |> GenState.all
        in
        let%bind destType = genType ~wrapInPtr:false type' in
        return @@ C.StructConstructor { type' = destType; args = elements }
      | Atom (Sigma _)
      | Atom (Literal IntLiteral)
      | Atom (Literal BooleanLiteral)
      | Atom (Literal CharacterLiteral) -> return @@ C.PtrDeref mem
    in
    let%bind mem = genMem ~hostOrDevice ~store:true addr in
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
      GenState.writeStatement
      @@ C.Define { name = UniqueName binding; type' = Some varType; value = Some value })
    |> GenState.all_unit
  in
  genBody body

and genMemLet
  : type l sIn sOut.
    hostOrDevice:l hostOrDevice
    -> genBody:(sIn -> (sOut, _) GenState.u)
    -> (l, sIn) Expr.memLet
    -> (sOut, _) GenState.u
  =
  fun ~hostOrDevice ~genBody { memArgs; body } ->
  let open GenState.Let_syntax in
  let%bind () =
    memArgs
    |> List.map ~f:(fun { memBinding; mem } ->
      let%bind varType = genType ~wrapInPtr:true @@ Mem.type' mem in
      let%bind mem = genMem ~hostOrDevice ~store:false mem in
      GenState.writeStatement
      @@ C.Define { name = UniqueName memBinding; type' = Some varType; value = Some mem })
    |> GenState.all_unit
  in
  genBody body
;;

let genMainBlock (main : withCaptures) =
  let open GenState.Let_syntax in
  let%bind cVal = genExpr ~hostOrDevice:Host ~store:false main in
  let%bind () = GenState.writeStatement (C.Eval cVal) in
  GenState.flushBlock ()
;;

let codegen (prog : withCaptures) : (CompilerState.state, C.t, _) State.t =
  let builder =
    let open CBuilder.Let_syntax in
    let%map _, block = GenState.run (genMainBlock prog) GenState.emptyState in
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
    CompilerPipeline.S.make ~f:(fun inState -> State.run (codegen input) inState)
  ;;
end
