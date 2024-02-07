open! Base

module Builder = struct
  type ('a, 'e) t =
    { value : 'a
    ; includesRev : string list
    ; funDecsRev : C.fun' C.declaration list
    ; structDecsRev : C.struct' C.declaration list
    }

  include Monad.Make2 (struct
      type nonrec ('a, 'e) t = ('a, 'e) t

      let bind start ~f =
        let finish = f start.value in
        { value = finish.value
        ; includesRev = finish.includesRev @ start.includesRev
        ; funDecsRev = finish.funDecsRev @ start.funDecsRev
        ; structDecsRev = finish.structDecsRev @ start.structDecsRev
        }
      ;;

      let map = `Define_using_bind
      let return value = { value; includesRev = []; funDecsRev = []; structDecsRev = [] }
    end)

  let addFunDec name funDec =
    { value = ()
    ; includesRev = []
    ; funDecsRev = [ { name; value = funDec } ]
    ; structDecsRev = []
    }
  ;;

  let addStructDec name structDec =
    { value = ()
    ; includesRev = []
    ; funDecsRev = []
    ; structDecsRev = [ { name; value = structDec } ]
    }
  ;;

  let addInclude include' =
    { value = (); includesRev = [ include' ]; funDecsRev = []; structDecsRev = [] }
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

let createId str =
  make ~f:(fun state ->
    State.run
      (Identifier.create
         str
         ~getCounter:(fun (s : CompilerState.state) -> s.idCounter)
         ~setCounter:(fun s idCounter -> { s with idCounter }))
      state)
;;

let createName : name -> (C.name, _) u = function
  | NameOfStr { str; needsUniquifying = true } ->
    let open Let_syntax in
    let%map id = createId str in
    C.Name.UniqueName id
  | NameOfStr { str; needsUniquifying = false } -> return @@ C.Name.StrName str
  | NameOfId id -> return @@ C.Name.UniqueName id
;;

let defineFunL name ~f =
  let open Let_syntax in
  let%bind cname = createName name in
  makeF ~f:(fun inState ->
    let open Builder.Let_syntax in
    let%bind state, (v, funDec) = run (f cname) inState in
    let%map () = Builder.addFunDec cname funDec in
    state, (v, cname))
;;

let defineFun name ~f =
  let open Let_syntax in
  let%map (), funDec =
    defineFunL name ~f:(fun n ->
      let%map funDec = f n in
      (), funDec)
  in
  funDec
;;

let defineStructL name ~f =
  let open Let_syntax in
  let%bind cname = createName name in
  makeF ~f:(fun inState ->
    let open Builder.Let_syntax in
    let%bind state, (v, structDec) = run (f cname) inState in
    let%map () = Builder.addStructDec cname structDec in
    state, (v, cname))
;;

let defineStruct name ~f =
  let open Let_syntax in
  let%map (), structDec =
    defineStructL name ~f:(fun n ->
      let%map structDec = f n in
      (), structDec)
  in
  structDec
;;

let include' str = returnF @@ Builder.addInclude str

let buildRaw ~prelude Builder.{ value = main; includesRev; funDecsRev; structDecsRev } =
  C.
    { includes = List.rev includesRev
    ; prelude
    ; funDecs = List.rev funDecsRev
    ; structDecs = List.rev structDecsRev
    ; main
    }
;;

let build ?(prelude = []) (prog : (C.block option, _) u)
  : (CompilerState.state, C.program, _) State.t
  =
  State.make ~f:(fun inState ->
    let Builder.{ value = outState, value; includesRev; funDecsRev; structDecsRev } =
      run prog inState
    in
    outState, buildRaw ~prelude { value; includesRev; funDecsRev; structDecsRev })
;;
