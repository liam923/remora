open! Base

module Builder = struct
  type ('a, 'e) t =
    { value : 'a
    ; funDecs : C.fun' C.declaration list
    ; structDecs : C.struct' C.declaration list
    }

  include Monad.Make2 (struct
      type nonrec ('a, 'e) t = ('a, 'e) t

      let bind _ ~f:_ = raise Unimplemented.default
      let map = `Define_using_bind
      let return value = { value; funDecs = []; structDecs = [] }
    end)

  let addFunDec name funDec =
    { value = (); funDecs = [ { name; value = funDec } ]; structDecs = [] }
  ;;

  let addStructDec name structDec =
    { value = (); funDecs = []; structDecs = [ { name; value = structDec } ] }
  ;;
end

include StateT.Make2 (Builder)

type ('a, 'e) u = (CompilerState.state, 'a, 'e) t

type name =
  | NameOfStr of
      { str : string
      ; needsUniquifying : bool
      }
  | NameOfId of Identifier.t

let createName : name -> (C.name, _) u = function
  | NameOfStr { str; needsUniquifying = true } ->
    let open Let_syntax in
    let%map id =
      make ~f:(fun state ->
        State.run
          (Identifier.create
             str
             ~getCounter:(fun (s : CompilerState.state) -> s.idCounter)
             ~setCounter:(fun s idCounter -> { s with idCounter }))
          state)
    in
    C.Name.UniqueName id
  | NameOfStr { str; needsUniquifying = false } -> return @@ C.Name.StrName str
  | NameOfId id -> return @@ C.Name.UniqueName id
;;

let defineFun name ~f =
  let open Let_syntax in
  let%bind cname = createName name in
  makeF ~f:(fun inState ->
    let open Builder.Let_syntax in
    let%bind state, funDec = run (f cname) inState in
    let%map () = Builder.addFunDec cname funDec in
    state, cname)
;;

let defineStruct name ~f =
  let open Let_syntax in
  let%bind cname = createName name in
  makeF ~f:(fun inState ->
    let open Builder.Let_syntax in
    let%bind state, structDec = run (f cname) inState in
    let%map () = Builder.addStructDec cname structDec in
    state, cname)
;;

let buildRaw Builder.{ value = main; funDecs; structDecs } =
  C.{ funDecs; structDecs; main }
;;

let build (prog : (C.block option, _) u) : (C.program, _) CompilerState.u =
  CompilerState.make ~f:(fun inState ->
    let Builder.{ value = outState, value; funDecs; structDecs } = run prog inState in
    outState, buildRaw { value; funDecs; structDecs })
;;
