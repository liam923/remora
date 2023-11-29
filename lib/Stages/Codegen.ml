open! Base
open Acorn

let prelude =
  {|#include <cstdio>

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
T *mallocHost(size_t count) {
  return (T *) malloc(count * sizeof(T));
};

template<typename T>
T *mallocDevice(size_t count) {
  T* array;
  HANDLE_ERROR(cudaMalloc((void **) &array, count * sizeof(T)));
  return array;
};

template<typename T>
T* copyToDevice(T* hostArray, size_t count) {
  T* deviceArray = mallocDevice<T>(count);
  HANDLE_ERROR(cudaMemcpy(deviceArray, hostArray, count * sizeof(T), cudaMemcpyHostToDevice));
  return deviceArray;
};

template<typename T>
T* copyToHost(T* deviceArray, size_t count) {
  T* hostArray = mallocHost<T>(count);
  HANDLE_ERROR(cudaMemcpy(hostArray, deviceArray, count * sizeof(T), cudaMemcpyDeviceToHost));
  return hostArray;
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
    }

  let emptyState = { statementsRev = []; typeCache = Map.empty (module Type) }

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
end

let tupleFieldName i = C.Name.StrName [%string "_%{i#Int}"]
let boxValueFieldName = C.Name.StrName "value"

let genTypeForSort sort =
  match sort with
  | Sort.Dim -> C.Int64
  | Sort.Shape -> C.Ptr C.Int64
;;

let rec unnestArray Type.{ element; size } =
  match element with
  | Literal _ | Sigma _ -> element, [ size ]
  | Array subArray ->
    let element, restShape = unnestArray subArray in
    element, size :: restShape
;;

let rec genType ?(wrapInPtr = false) type' : (C.type', _) GenState.u =
  let open GenState.Let_syntax in
  let%bind (state : GenState.state) = GenState.get () in
  let typeCache = state.typeCache in
  match Map.find typeCache type' with
  | Some cType -> return cType
  | None ->
    let%bind cType =
      match type' with
      | Type.NonTuple (Literal IntLiteral) ->
        return @@ if wrapInPtr then C.Ptr C.Int64 else C.Int64
      | Type.NonTuple (Literal CharacterLiteral) ->
        return @@ if wrapInPtr then C.Ptr C.Char else C.Char
      | Type.NonTuple (Literal BooleanLiteral) ->
        return @@ if wrapInPtr then C.Ptr C.Bool else C.Bool
      | Type.NonTuple (Array array) ->
        let elementType, _ = unnestArray array in
        let%map elementCType = genType ~wrapInPtr:false (NonTuple elementType) in
        C.Ptr elementCType
      | Type.NonTuple (Sigma { parameters; body }) ->
        let%map name =
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
        if wrapInPtr then C.Ptr (C.TypeRef name) else C.TypeRef name
      | Type.Tuple elements ->
        let%bind elementTypes = elements |> List.map ~f:genType |> GenState.all in
        let%map name =
          GenState.defineStruct
            (NameOfStr { str = "Tuple"; needsUniquifying = true })
            ~f:(fun _ ->
              return
              @@ List.mapi elementTypes ~f:(fun i type' ->
                C.{ name = tupleFieldName i; type' }))
        in
        if wrapInPtr then C.Ptr (C.TypeRef name) else C.TypeRef name
    in
    let%bind (state : GenState.state) = GenState.get () in
    let updatedState =
      { state with typeCache = Map.set state.typeCache ~key:type' ~data:cType }
    in
    let%map () = GenState.set updatedState in
    cType
;;

let rec genHostMem (mem : host Mem.t) : (C.expr, _) GenState.u =
  let open GenState.Let_syntax in
  match mem with
  | Ref { id; type' = _ } -> return @@ C.VarRef (UniqueName id)
  | Malloc _ -> raise Unimplemented.default
  | Values { elements; type' } ->
    let%bind elements = elements |> List.map ~f:genHostMem |> GenState.all in
    let%map type' = genType ~wrapInPtr:true type' in
    C.StructConstructor { type'; args = elements }
  | TupleDeref { tuple; index; type' = _ } ->
    let%map tuple = genHostMem tuple in
    C.FieldDeref { value = tuple; fieldName = tupleFieldName index }
  | Index _ -> raise Unimplemented.default
;;

let rec genHostStmnt (stmnt : (host, Expr.captures) Expr.statement) : (unit, _) GenState.u
  =
  let open GenState.Let_syntax in
  match stmnt with
  | Statements statements -> statements |> List.map ~f:genHostStmnt |> GenState.all_unit
  | Putmem _ -> raise Unimplemented.default
  | MapKernel _ -> raise Unimplemented.default
  | ComputeForSideEffects expr ->
    let%bind expr = genHostExpr expr in
    GenState.writeStatement @@ C.Eval expr
  | SLet let' -> genHostLet ~genBody:genHostStmnt let'
  | SMemLet memLet -> genHostMemLet ~genBody:genHostStmnt memLet
  | ReifyShapeIndexToBox _ -> raise Unimplemented.default
  | ReifyShapeIndexToArray _ -> raise Unimplemented.default

and genHostExpr (expr : host Expr.withCaptures) : (C.expr, _) GenState.u =
  expr
  |> [%sexp_of: (host, Expr.captures) Expr.t]
  |> Sexp.to_string_hum
  |> Stdio.print_endline;
  let open GenState.Let_syntax in
  match expr with
  | Box _ -> raise Unimplemented.default
  | Literal (IntLiteral i) -> return @@ C.Literal (Int64Literal i)
  | Literal (CharacterLiteral c) -> return @@ C.Literal (CharLiteral c)
  | Literal (BooleanLiteral b) -> return @@ C.Literal (BoolLiteral b)
  | ScalarPrimitive _ -> raise Unimplemented.default
  | TupleDeref { tuple; index; type' = _ } ->
    let%map tuple = genHostExpr tuple in
    C.FieldDeref { value = tuple; fieldName = tupleFieldName index }
  | Values _ -> raise Unimplemented.default
  | Ref { id; type' = _ } -> return @@ C.VarRef (UniqueName id)
  | BoxValue { box; type' = _ } ->
    let%map box = genHostExpr box in
    C.FieldDeref { value = box; fieldName = boxValueFieldName }
  | IndexLet _ -> raise Unimplemented.default
  | Let let' -> genHostLet ~genBody:genHostExpr let'
  | MemLet memLet -> genHostMemLet ~genBody:genHostExpr memLet
  | ReifyDimensionIndex _ -> raise Unimplemented.default
  | LoopBlock _ -> raise Unimplemented.default
  | LoopKernel _ -> raise Unimplemented.default
  | SubArray _ -> raise Unimplemented.default
  | IfParallelismHitsCutoff _ -> raise Unimplemented.default
  | Eseq { statement; expr; type' = _ } ->
    let%bind () = genHostStmnt statement in
    genHostExpr expr
  | Getmem _ -> raise Unimplemented.default

and genHostLet
  : type sIn sOut.
    genBody:(sIn -> (sOut, _) GenState.u)
    -> (host, sIn, Expr.captures) Expr.let'
    -> (sOut, _) GenState.u
  =
  fun ~genBody Expr.{ args; body } ->
  let open GenState.Let_syntax in
  let%bind () =
    args
    |> List.map ~f:(fun { binding; value } ->
      let%bind varType = genType @@ Expr.type' value in
      let%bind value = genHostExpr value in
      GenState.writeStatement
      @@ C.Define { name = UniqueName binding; type' = varType; value = Some value })
    |> GenState.all_unit
  in
  genBody body

and genHostMemLet
  : type sIn sOut.
    genBody:(sIn -> (sOut, _) GenState.u)
    -> (host, sIn) Expr.memLet
    -> (sOut, _) GenState.u
  =
  fun ~genBody { memArgs; body } ->
  let open GenState.Let_syntax in
  let%bind () =
    memArgs
    |> List.map ~f:(fun { memBinding; mem } ->
      let%bind varType = genType @@ Mem.type' mem in
      let%bind mem = genHostMem mem in
      GenState.writeStatement
      @@ C.Define { name = UniqueName memBinding; type' = varType; value = Some mem })
    |> GenState.all_unit
  in
  genBody body
;;

let genMainBlock (main : withCaptures) =
  let open GenState.Let_syntax in
  let%bind cVal = genHostExpr main in
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
