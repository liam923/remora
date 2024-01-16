open! Base
open MResult
open MResult.Let_syntax
open Ast

module type S = sig
  type source
  type error = string * source
  type 't result = ('t, error) MResult.t

  module type Parser = sig
    type t

    val parseEsexps : source Esexp.t NeList.t -> t result
    val parseString : string -> t result
    val parseFile : string -> t result

    module Stage :
      CompilerPipeline.Stage
        with type state = CompilerState.state
        with type input = string
        with type output = t
        with type error = (source option, string) Source.annotate
  end

  module IndexParser : Parser with type t = source Index.t
  module TypeParser : Parser with type t = source Type.t
  module ExprParser : Parser with type t = source Expr.t
  include module type of ExprParser
end

module Make (SB : Source.BuilderT) = struct
  type source = SB.source

  module Lexer = EsexpLexer.Make (SB)
  module Parser = EsexpParser.Make (SB)

  type error = string * SB.source
  type 't result = ('t, error) MResult.t

  let esexpSource = Esexp.source (module SB)

  let infixNeListSource list =
    SB.merge (esexpSource (NeList.hd list)) (esexpSource (NeList.last list))
  ;;

  let infixListSource list ~before ~after =
    match list with
    | [] -> SB.between before after
    | head :: tail -> infixNeListSource (head :: tail)
  ;;

  let parseInfixList list ~f ~before ~after =
    let%map parsedElements = list |> List.map ~f |> MResult.all in
    Source.{ elem = parsedElements; source = infixListSource list ~before ~after }
  ;;

  let parseInfixNeList list ~f =
    let%map parsedElements = list |> NeList.map ~f |> MResult.allNE in
    Source.{ elem = parsedElements; source = infixNeListSource list }
  ;;

  let parseList list ~f ~source =
    let%map parsedElements = list |> List.map ~f |> MResult.all in
    Source.{ elem = parsedElements; source }
  ;;

  let parseNeList list ~f ~source =
    let%map parsedElements = list |> NeList.map ~f |> MResult.allNE in
    Source.{ elem = parsedElements; source }
  ;;

  let parseBindingWithImplicitBound esexp ~atBound ~noAtBound =
    match esexp with
    | Esexp.Symbol (id, source) ->
      let bound = if String.is_prefix id ~prefix:"@" then atBound else noAtBound in
      MOk (Source.{ elem = id; source }, Source.{ elem = bound; source }, source)
    | param -> MResult.err ("Expected an identifier", esexpSource param)
  ;;

  let parseBinding = function
    | Esexp.Symbol (id, source) ->
      if String.is_prefix id ~prefix:"@"
      then MResult.err ("Expected an identifier without an @", source)
      else MOk Source.{ elem = id; source }
    | param -> MResult.err ("Expected an identifier", esexpSource param)
  ;;

  let rec parseIndex : 's Esexp.t -> ('s Index.t, error) MResult.t = function
    | Integer (i, source) -> MOk { elem = Index.Dimension i; source }
    | Symbol (id, source) -> MOk { elem = Index.Ref id; source }
    (* Match (+ ...) *)
    | ParenList { elements = Symbol ("+", _) :: indices; braceSources = _ } as addExp ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(esexpSource addExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Add indices)
    (* Match (++ ...) *)
    | ParenList { elements = Symbol ("++", _) :: indices; braceSources = _ } as appendExp
      ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(esexpSource appendExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Append indices)
    (* Match (shape ...) *)
    | ParenList { elements = Symbol ("shape", _) :: indices; braceSources = _ } as
      shapeExp ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(esexpSource shapeExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Shape indices)
    (* Match [...]
       Note that the de-sugaring is performed when type checking *)
    | SquareList { elements = indices; braceSources = _ } as shapeExp ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(esexpSource shapeExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Slice indices)
    | index -> MResult.err ("Bad index syntax", esexpSource index)

  and parseType : 's Esexp.t -> ('s Type.t, error) MResult.t =
    (* A function used for parsing Forall, Pi, and Sigma *)
    let parseAbstraction
      (type k)
      (components : source Esexp.t list)
      ~(atBound : k)
      ~(noAtBound : k)
      ~symbol
      ~symbolSource
      ~rParenSource
      =
      match components with
      | [ (ParenList { elements = parameters; braceSources = _ } as paramsExp); body ] ->
        let%map parsedBody = parseType body
        and parsedParameters =
          parseList
            parameters
            ~f:(fun param ->
              let%map binding, bound, source =
                parseBindingWithImplicitBound param ~atBound ~noAtBound
              in
              Source.{ elem = { binding; bound }; source })
            ~source:(esexpSource paramsExp)
        in
        Type.{ parameters = parsedParameters; body = parsedBody }
      | _ ->
        MResult.err
          ( [%string "Bad `%{symbol}` syntax"]
          , infixListSource components ~before:symbolSource ~after:rParenSource )
    in
    function
    | Symbol (id, source) -> MOk { elem = Type.Ref id; source }
    | ParenList
        { elements = Symbol ("Arr", arrSource) :: components
        ; braceSources = _, rParenSource
        } as arrExp ->
      (match components with
       | [ elementType; shape ] ->
         let%map parsedElementType = parseType elementType
         and parsedShape = parseIndex shape in
         Source.
           { elem = Type.Arr { element = parsedElementType; shape = parsedShape }
           ; source = esexpSource arrExp
           }
       | _ ->
         MResult.err
           ( "Bad `Arr` syntax"
           , infixListSource components ~before:arrSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol (arrow, arrSource) :: components
        ; braceSources = _, rParenSource
        } as arrExp
      when String.equal arrow "->" || String.equal arrow "→" ->
      (match components with
       | [ (ParenList { elements = parameters; braceSources = _ } as paramsExp); return ]
         ->
         let%map parsedParameters =
           parseList parameters ~f:parseType ~source:(esexpSource paramsExp)
         and parsedReturn = parseType return in
         Source.
           { elem = Type.Func { parameters = parsedParameters; return = parsedReturn }
           ; source = esexpSource arrExp
           }
       | _ ->
         MResult.err
           ( [%string "Bad `%{arrow}` syntax"]
           , infixListSource components ~before:arrSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol (forall, forallSource) :: components
        ; braceSources = _, rParenSource
        } as forallExp
      when String.equal forall "Forall" || String.equal forall "∀" ->
      let%map abstraction =
        parseAbstraction
          components
          ~atBound:Kind.Array
          ~noAtBound:Kind.Atom
          ~symbol:forall
          ~symbolSource:forallSource
          ~rParenSource
      in
      Source.{ elem = Type.Forall abstraction; source = esexpSource forallExp }
    | ParenList
        { elements = Symbol (pi, piSource) :: components; braceSources = _, rParenSource }
      as piExp
      when String.equal pi "Pi" || String.equal pi "Π" ->
      let%map abstraction =
        parseAbstraction
          components
          ~atBound:Sort.Shape
          ~noAtBound:Sort.Dim
          ~symbol:pi
          ~symbolSource:piSource
          ~rParenSource
      in
      Source.{ elem = Type.Pi abstraction; source = esexpSource piExp }
    | ParenList
        { elements = Symbol (sigma, sigmaSource) :: components
        ; braceSources = _, rParenSource
        } as sigmaExp
      when String.equal sigma "Sigma" || String.equal sigma "Σ" ->
      let%map abstraction =
        parseAbstraction
          components
          ~atBound:Sort.Shape
          ~noAtBound:Sort.Dim
          ~symbol:sigma
          ~symbolSource:sigmaSource
          ~rParenSource
      in
      Source.{ elem = Type.Sigma abstraction; source = esexpSource sigmaExp }
    | SquareList
        { elements = elementType :: shapeElements; braceSources = _, rBrackSource } as
      arrExp ->
      let%map parsedElementType = parseType elementType
      and parsedShapeElements =
        parseInfixList
          shapeElements
          ~f:parseIndex
          ~before:(esexpSource elementType)
          ~after:rBrackSource
      in
      Source.
        { elem =
            Type.Arr
              { element = parsedElementType
              ; shape = Source.map parsedShapeElements ~f:(fun elems -> Index.Slice elems)
              }
        ; source = esexpSource arrExp
        }
    | type' -> MResult.err ("Bad type syntax", esexpSource type')

  and parseExpr : 's Esexp.t -> ('s Expr.t, error) MResult.t =
    (* array and frame syntax is very similar and both are complex,
       so this abstracts out the commonality *)
    let parseArrayOrFrame
      (components : source Esexp.t list)
      ~arrOrFrameExp
      ~symbol
      ~symbolSource
      ~rParenSource
      =
      match components with
      | (SquareList { elements = dimensions; braceSources = _ } as dimsExp) :: elements ->
        let%bind parsedDimensions =
          parseList
            dimensions
            ~f:(function
              | Integer (dim, dimSource) -> MOk Source.{ elem = dim; source = dimSource }
              | _ -> Error.raise (Error.of_string "unimplemented"))
            ~source:(esexpSource dimsExp)
        in
        (match List.find parsedDimensions.elem ~f:(fun dim -> dim.elem = 0) with
         | None ->
           (match NeList.of_list elements with
            | Some elements ->
              let%map parsedElements = parseInfixNeList elements ~f:parseExpr in
              Source.
                { elem =
                    `NonEmpty
                      Expr.{ dimensions = parsedDimensions; elements = parsedElements }
                ; source = esexpSource arrOrFrameExp
                }
            | None ->
              MResult.err
                ( [%string "Bad `%{symbol}` syntax - not enough elements"]
                , SB.between (esexpSource dimsExp) rParenSource ))
         | Some _ ->
           (match elements with
            | [ elementType ] ->
              let%map parsedElementType = parseType elementType in
              Source.
                { elem =
                    `Empty
                      Expr.
                        { elementType = parsedElementType; dimensions = parsedDimensions }
                ; source = esexpSource arrOrFrameExp
                }
            | _ ->
              MResult.err
                ( [%string "Bad `%{symbol}` syntax - expected element type"]
                , infixListSource
                    elements
                    ~before:(esexpSource dimsExp)
                    ~after:rParenSource )))
      | _ ->
        MResult.err
          ( [%string "Bad `%{symbol}` syntax"]
          , infixListSource components ~before:symbolSource ~after:rParenSource )
    in
    function
    | Symbol ("#t", source) | Symbol ("#true", source) ->
      MOk { elem = Expr.BooleanLiteral true; source }
    | Symbol ("#f", source) | Symbol ("#false", source) ->
      MOk { elem = Expr.BooleanLiteral false; source }
    | Symbol (id, source) -> MOk { elem = Expr.Ref id; source }
    | Integer (i, source) -> MOk { elem = Expr.IntLiteral i; source }
    | Float (f, source) -> MOk { elem = Expr.FloatLiteral f; source }
    | String (str, source) ->
      (match String.to_list str with
       | [] ->
         MOk
           { elem =
               Expr.EmptyArr
                 { dimensions = { elem = [ { elem = 0; source } ]; source }
                 ; elementType = { elem = Type.Ref "char"; source }
                 }
           ; source
           }
       | head :: tail ->
         MOk
           { elem =
               Expr.Arr
                 { dimensions =
                     { elem = [ { elem = List.length (head :: tail); source } ]; source }
                 ; elements =
                     { elem =
                         NeList.map (head :: tail) ~f:(fun c ->
                           Source.{ elem = Expr.CharacterLiteral c; source })
                     ; source
                     }
                 }
           ; source
           })
    | SquareList { elements; braceSources = _ } as frameExp ->
      let source = esexpSource frameExp in
      (match elements with
       | head :: tail ->
         let%map parsedElements =
           parseNeList (head :: tail) ~f:parseExpr ~source:(esexpSource frameExp)
         in
         Source.
           { elem =
               Expr.Frame
                 { dimensions =
                     { elem = [ { elem = NeList.length parsedElements.elem; source } ]
                     ; source
                     }
                 ; elements = parsedElements
                 }
           ; source
           }
       | [] -> MResult.err ("[] sugar can only be used for non-empty frames", source))
    | ParenList
        { elements = Symbol ("array", arrSource) :: components
        ; braceSources = _, rParenSource
        } as arrExp ->
      let%map { elem = result; source } =
        parseArrayOrFrame
          components
          ~arrOrFrameExp:arrExp
          ~symbol:"array"
          ~symbolSource:arrSource
          ~rParenSource
      in
      (match result with
       | `NonEmpty arr -> Source.{ elem = Expr.Arr arr; source }
       | `Empty empty -> Source.{ elem = Expr.EmptyArr empty; source })
    | ParenList
        { elements = Symbol ("frame", frameSource) :: components
        ; braceSources = _, rParenSource
        } as frameExp ->
      let%map { elem = result; source } =
        parseArrayOrFrame
          components
          ~arrOrFrameExp:frameExp
          ~symbol:"frame"
          ~symbolSource:frameSource
          ~rParenSource
      in
      (match result with
       | `NonEmpty frame -> Source.{ elem = Expr.Frame frame; source }
       | `Empty empty -> Source.{ elem = Expr.EmptyFrame empty; source })
    | ParenList
        { elements = Symbol ("t-app", tAppSource) :: components
        ; braceSources = _, rParenSource
        } as tAppExpr ->
      (match components with
       | tFunc :: args ->
         let%map tFuncParsed = parseExpr tFunc
         and argsParsed =
           parseInfixList
             args
             ~f:parseType
             ~before:(esexpSource tFunc)
             ~after:rParenSource
         in
         Source.
           { elem = Expr.TypeApplication { tFunc = tFuncParsed; args = argsParsed }
           ; source = esexpSource tAppExpr
           }
       | _ ->
         MResult.err
           ( "Bad `t-app` syntax"
           , infixListSource components ~before:tAppSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("i-app", iAppSource) :: components
        ; braceSources = _, rParenSource
        } as iAppExpr ->
      (match components with
       | iFunc :: args ->
         let%map iFuncParsed = parseExpr iFunc
         and argsParsed =
           parseInfixList
             args
             ~f:parseIndex
             ~before:(esexpSource iFunc)
             ~after:rParenSource
         in
         Source.
           { elem = Expr.IndexApplication { iFunc = iFuncParsed; args = argsParsed }
           ; source = esexpSource iAppExpr
           }
       | _ ->
         MResult.err
           ( "Bad `i-app` syntax"
           , infixListSource components ~before:iAppSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("unbox", unboxSource) :: components
        ; braceSources = _, rParenSource
        } as unboxExpr ->
      (match components with
       | box
         :: ParenList
              { elements = valueBinding :: indexBindings
              ; braceSources = _, bindingsRParen
              }
         :: bodyHead
         :: bodyTail ->
         let parseIndexBinding indexBinding =
           let%map binding, bound, source =
             parseBindingWithImplicitBound
               indexBinding
               ~atBound:Sort.Shape
               ~noAtBound:Sort.Dim
           in
           Source.{ elem = Expr.{ binding; bound = Some bound }; source }
         in
         let%map parsedIndexBindings =
           parseInfixList
             indexBindings
             ~f:parseIndexBinding
             ~before:(esexpSource valueBinding)
             ~after:bindingsRParen
         and parsedValueBinding = parseBinding valueBinding
         and parsedBox = parseExpr box
         and parsedBody = parseExprBody (bodyHead :: bodyTail) in
         Source.
           { elem =
               Expr.Unbox
                 { indexBindings = parsedIndexBindings
                 ; valueBinding = parsedValueBinding
                 ; box = parsedBox
                 ; body = parsedBody
                 }
           ; source = esexpSource unboxExpr
           }
       | _ ->
         MResult.err
           ( "Bad `unbox` syntax"
           , infixListSource components ~before:unboxSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("boxes", boxesSource) :: components
        ; braceSources = _, rParenSource
        } as boxesExpr ->
      (match components with
       | (ParenList { elements = params; braceSources = _ } as paramsExp)
         :: elementType
         :: (SquareList { elements = dimensions; braceSources = _ } as dimsExp)
         :: elements ->
         let parseElement = function
           | Esexp.ParenList
               { elements =
                   ParenList { elements = indices; braceSources = _ }
                   :: bodyHead
                   :: bodyTail
               ; braceSources = indicesLParen, indicesRParen
               } as elementExp ->
             let%map parsedIndices =
               parseInfixList
                 indices
                 ~f:parseIndex
                 ~before:indicesLParen
                 ~after:indicesRParen
             and parsedBody = parseExprBody (bodyHead :: bodyTail) in
             Source.
               { elem = Expr.{ indices = parsedIndices; body = parsedBody }
               ; source = esexpSource elementExp
               }
           | elementExp ->
             MResult.err
               ("Bad `boxes` syntax - expected a box element", esexpSource elementExp)
         in
         let parseDimension = function
           | Esexp.Integer (i, source) -> MOk Source.{ elem = i; source }
           | dim -> MResult.err ("Expected an integer", esexpSource dim)
         in
         let%map parsedParams =
           parseList
             params
             ~f:(fun param ->
               let%map binding, bound, source =
                 parseBindingWithImplicitBound
                   param
                   ~atBound:Sort.Shape
                   ~noAtBound:Sort.Dim
               in
               Source.{ elem = { binding; bound }; source })
             ~source:(esexpSource paramsExp)
         and parsedElementType = parseType elementType
         and parsedDimensions =
           parseList dimensions ~f:parseDimension ~source:(esexpSource dimsExp)
         and parsedElements =
           parseInfixList
             elements
             ~f:parseElement
             ~before:(esexpSource dimsExp)
             ~after:rParenSource
         in
         Source.
           { elem =
               Expr.Boxes
                 { params = parsedParams
                 ; elementType = parsedElementType
                 ; dimensions = parsedDimensions
                 ; elements = parsedElements
                 }
           ; source = esexpSource boxesExpr
           }
       | _ ->
         MResult.err
           ( "Bad `boxes` syntax"
           , infixListSource components ~before:boxesSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("box", boxSource) :: components
        ; braceSources = _, rParenSource
        } as boxExpr ->
      (match components with
       | (ParenList { elements = args; braceSources = _ } as argsExp)
         :: elementType
         :: bodyHead
         :: bodyTail ->
         let parseArg = function
           | Esexp.ParenList { elements = [ param; index ]; braceSources = _ } ->
             let%map paramBinding, paramBound, paramSource =
               parseBindingWithImplicitBound param ~atBound:Sort.Shape ~noAtBound:Sort.Dim
             and parsedIndex = parseIndex index in
             ( Source.
                 { elem = { binding = paramBinding; bound = paramBound }
                 ; source = paramSource
                 }
             , parsedIndex )
           | argExp ->
             MResult.err ("Bad `box` syntax - expected an arguement", esexpSource argExp)
         in
         let%map parsedParamsAndIndices =
           parseList args ~f:parseArg ~source:(esexpSource argsExp)
         and parsedElementType = parseType elementType
         and parsedBody = parseExprBody (bodyHead :: bodyTail) in
         let parsedParams, parsedIndices =
           parsedParamsAndIndices |> Source.map ~f:List.unzip |> Source.unzip
         in
         Source.
           { elem =
               Expr.Boxes
                 { params = parsedParams
                 ; elementType = parsedElementType
                 ; dimensions = { elem = []; source = esexpSource boxExpr }
                 ; elements =
                     { elem =
                         [ { elem = { indices = parsedIndices; body = parsedBody }
                           ; source = esexpSource boxExpr
                           }
                         ]
                     ; source = esexpSource boxExpr
                     }
                 }
           ; source = esexpSource boxExpr
           }
       | _ ->
         MResult.err
           ( "Bad `box` syntax"
           , infixListSource components ~before:boxSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("lift", liftSource) :: components
        ; braceSources = _, rParenSource
        } as liftExpr ->
      (match components with
       | SquareList { elements = [ param; value ]; braceSources = _ }
         :: bodyHead
         :: bodyRest ->
         let%map paramBinding, paramSort, _ =
           parseBindingWithImplicitBound param ~atBound:Sort.Shape ~noAtBound:Sort.Dim
         and parsedValue = parseExpr value
         and parsedBody = parseExprBody (bodyHead :: bodyRest) in
         Source.
           { elem =
               Expr.Lift
                 { indexBinding = paramBinding
                 ; indexValue = parsedValue
                 ; sort = paramSort
                 ; body = parsedBody
                 }
           ; source = esexpSource liftExpr
           }
       | _ ->
         MResult.err
           ( "Bad `lift` syntax"
           , infixListSource components ~before:liftSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("reshape", reshapeSource) :: components
        ; braceSources = _, rParenSource
        } as reshapeExpr ->
      (match components with
       | [ newShape; value ] ->
         let%map newShape = parseIndex newShape
         and value = parseExpr value in
         Source.
           { elem = Expr.Reshape { newShape; value }; source = esexpSource reshapeExpr }
       | _ ->
         MResult.err
           ( "Bad `reshape` syntax"
           , infixListSource components ~before:reshapeSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("reify-dimension", reifySource) :: components
        ; braceSources = _, rParenSource
        } as reifyExpr ->
      (match components with
       | [ dimension ] ->
         let%map dimension = parseIndex dimension in
         Source.{ elem = Expr.ReifyDimension dimension; source = esexpSource reifyExpr }
       | _ ->
         MResult.err
           ( "Bad `reify` syntax"
           , infixListSource components ~before:reifySource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("reify-shape", reifySource) :: components
        ; braceSources = _, rParenSource
        } as reifyExpr ->
      (match components with
       | [ shape ] ->
         let%map shape = parseIndex shape in
         Source.{ elem = Expr.ReifyShape shape; source = esexpSource reifyExpr }
       | _ ->
         MResult.err
           ( "Bad `reify` syntax"
           , infixListSource components ~before:reifySource ~after:rParenSource ))
    | ParenList
        { elements = Symbol ("let", letSource) :: components
        ; braceSources = _, rParenSource
        } as letExpr ->
      (match components with
       | (SquareList { elements = declarationComponents; braceSources = _ } as decExp)
         :: bodyHead
         :: bodyTail ->
         let%map parsedBody = parseExprBody (bodyHead :: bodyTail)
         and parsedParam, parsedValue =
           match declarationComponents with
           | [ binding; value ] ->
             let%map parsedBinding = parseBinding binding
             and parsedValue = parseExpr value in
             let parsedParam =
               Source.
                 { elem = { binding = parsedBinding; bound = None }
                 ; source = parsedBinding.source
                 }
             in
             parsedParam, parsedValue
           | [ binding; Symbol (":", _); bound; value ] ->
             let%map parsedBinding = parseBinding binding
             and parsedBound = parseType bound
             and parsedValue = parseExpr value in
             let parsedParam =
               Source.
                 { elem = { binding = parsedBinding; bound = Some parsedBound }
                 ; source = SB.merge parsedBinding.source parsedBound.source
                 }
             in
             parsedParam, parsedValue
           | _ -> MResult.err ("Bad `let` syntax - bad declaration", esexpSource decExp)
         in
         Source.
           { elem =
               Expr.Let { param = parsedParam; value = parsedValue; body = parsedBody }
           ; source = esexpSource letExpr
           }
       | _ ->
         MResult.err
           ( "Bad `let` syntax"
           , infixListSource components ~before:letSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol (lambda, lambdaSource) :: components
        ; braceSources = _, rParenSource
        } as lambdaExp
      when String.equal lambda "λ" || String.equal lambda "fn" ->
      (match components with
       | (ParenList { elements = params; braceSources = _ } as paramsExp)
         :: bodyHead
         :: bodyTail ->
         let parseParam = function
           | Esexp.SquareList { elements = [ binding; bound ]; braceSources = _ } as
             paramExp ->
             let%map parsedBinding = parseBinding binding
             and parsedBound = parseType bound in
             Source.
               { elem = { binding = parsedBinding; bound = parsedBound }
               ; source = esexpSource paramExp
               }
           | paramExp ->
             MResult.err
               ([%string "Bad `%{lambda}` syntax - bad parameter"], esexpSource paramExp)
         in
         let%map parsedParams =
           parseList params ~f:parseParam ~source:(esexpSource paramsExp)
         and parsedBody = parseExprBody (bodyHead :: bodyTail) in
         Source.
           { elem = Expr.TermLambda { params = parsedParams; body = parsedBody }
           ; source = esexpSource lambdaExp
           }
       | _ ->
         MResult.err
           ( [%string "Bad `%{lambda}` syntax"]
           , infixListSource components ~before:lambdaSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol (tlambda, tlambdaSource) :: components
        ; braceSources = _, rParenSource
        } as tlambdaExp
      when String.equal tlambda "Tλ" || String.equal tlambda "t-fn" ->
      (match components with
       | (ParenList { elements = params; braceSources = _ } as paramsExp)
         :: bodyHead
         :: bodyTail ->
         let parseParam param =
           let%map binding, bound, source =
             parseBindingWithImplicitBound param ~atBound:Kind.Array ~noAtBound:Kind.Atom
           in
           Source.{ elem = { binding; bound }; source }
         in
         let%map parsedParams =
           parseList params ~f:parseParam ~source:(esexpSource paramsExp)
         and parsedBody = parseExprBody (bodyHead :: bodyTail) in
         Source.
           { elem = Expr.TypeLambda { params = parsedParams; body = parsedBody }
           ; source = esexpSource tlambdaExp
           }
       | _ ->
         MResult.err
           ( [%string "Bad `%{tlambda}` syntax"]
           , infixListSource components ~before:tlambdaSource ~after:rParenSource ))
    | ParenList
        { elements = Symbol (ilambda, ilambdaSource) :: components
        ; braceSources = _, rParenSource
        } as ilambdaExp
      when String.equal ilambda "Iλ" || String.equal ilambda "i-fn" ->
      (match components with
       | (ParenList { elements = params; braceSources = _ } as paramsExp)
         :: bodyHead
         :: bodyTail ->
         let parseParam param =
           let%map binding, bound, source =
             parseBindingWithImplicitBound param ~atBound:Sort.Shape ~noAtBound:Sort.Dim
           in
           Source.{ elem = { binding; bound }; source }
         in
         let%map parsedParams =
           parseList params ~f:parseParam ~source:(esexpSource paramsExp)
         and parsedBody = parseExprBody (bodyHead :: bodyTail) in
         Source.
           { elem = Expr.IndexLambda { params = parsedParams; body = parsedBody }
           ; source = esexpSource ilambdaExp
           }
       | _ ->
         MResult.err
           ( [%string "Bad `%{ilambda}` syntax"]
           , infixListSource components ~before:ilambdaSource ~after:rParenSource ))
    | ParenList { elements = func :: args; braceSources = _, rParenSource } as termAppExp
      ->
      let%map parsedFunc = parseExpr func
      and parsedArgs =
        parseInfixList args ~f:parseExpr ~before:(esexpSource func) ~after:rParenSource
      in
      Source.
        { elem = Expr.TermApplication { func = parsedFunc; args = parsedArgs }
        ; source = esexpSource termAppExp
        }
    | WithCurlies
        { base
        ; leftElements
        ; rightElements
        ; curlySources = lCurlySource, rCurlySource
        ; splitSource
        } as curliesExp ->
      let%map expression = parseExpr base
      and iAppArgs =
        parseInfixList rightElements ~f:parseIndex ~before:splitSource ~after:rCurlySource
      and tAppArgs =
        parseInfixList leftElements ~f:parseType ~before:lCurlySource ~after:splitSource
      in
      (* If there are elements on the right half, parse them as indices and
         wrap an i-app around the expression *)
      let expression =
        match iAppArgs with
        | { elem = []; source = _ } -> expression
        | { elem = _ :: _; source = _ } as iAppArgs ->
          Source.
            { elem = Expr.IndexApplication { iFunc = expression; args = iAppArgs }
            ; source = esexpSource curliesExp
            }
      in
      (* If there are elements on the left half, parse them as types and
         wrap a t-app around the expression *)
      let expression =
        match tAppArgs with
        | { elem = []; source = _ } -> expression
        | { elem = _ :: _; source = _ } as tAppArgs ->
          Source.
            { elem = Expr.TypeApplication { tFunc = expression; args = tAppArgs }
            ; source = esexpSource curliesExp
            }
      in
      expression
    | expr -> MResult.err ("Bad expression syntax", esexpSource expr)

  (* Parse a list of define statements followed by a expression *)
  and parseExprBody : source Esexp.t NeList.t -> (source Expr.t, error) MResult.t
    = function
    | [ body ] -> parseExpr body
    | define :: next :: rest ->
      let body : source Esexp.t NeList.t = next :: rest in
      let letSource = infixNeListSource (define :: next :: rest) in
      let parseDefineBinding = function
        | Esexp.WithCurlies
            { base = binding
            ; leftElements
            ; rightElements
            ; curlySources = lCurlySource, rCurlySource
            ; splitSource
            } ->
          let%map binding = parseBinding binding
          and typeBindings =
            parseInfixList
              leftElements
              ~f:(parseBindingWithImplicitBound ~atBound:Kind.Array ~noAtBound:Kind.Atom)
              ~before:splitSource
              ~after:rCurlySource
          and indexBindings =
            parseInfixList
              rightElements
              ~f:(parseBindingWithImplicitBound ~atBound:Sort.Shape ~noAtBound:Sort.Dim)
              ~before:lCurlySource
              ~after:splitSource
          in
          let wrapBody body =
            (* If there are elements on the left half, parse them as types and
               wrap a t-fn around the expression *)
            let body =
              match typeBindings with
              | { elem = []; source = _ } -> body
              | { elem = params; source = paramsSource } ->
                let params =
                  Source.
                    { elem =
                        List.map params ~f:(fun (binding, bound, source) ->
                          Source.{ elem = { binding; bound }; source })
                    ; source = paramsSource
                    }
                in
                Source.{ elem = Expr.TypeLambda { params; body }; source = letSource }
            in
            (* If there are elements on the right half, parse them as index
               variables and wrap an i-fn around the body *)
            let body =
              match indexBindings with
              | { elem = []; source = _ } -> body
              | { elem = params; source = paramsSource } ->
                let params =
                  Source.
                    { elem =
                        List.map params ~f:(fun (binding, bound, source) ->
                          Source.{ elem = { binding; bound }; source })
                    ; source = paramsSource
                    }
                in
                Source.{ elem = Expr.IndexLambda { params; body }; source = letSource }
            in
            body
          in
          binding, wrapBody
        | binding ->
          let%map binding = parseBinding binding in
          binding, fun exp -> exp
      in
      let%map parsedBody = parseExprBody body
      and parsedDefine =
        match define with
        | ParenList
            { elements = Symbol ("define", defineSource) :: components
            ; braceSources = _, defineRParenSource
            } as defineExp ->
          (match components with
           (* Match a define for a function *)
           | ParenList { elements = funBinding :: params; braceSources = _, rParenSource }
             :: functionBodyHead
             :: functionBodyTail ->
             let%map parsedFunctionBody =
               parseExprBody (functionBodyHead :: functionBodyTail)
             and parsedFunBinding, wrapFun = parseDefineBinding funBinding
             and parsedParams =
               parseInfixList
                 params
                 ~f:(function
                   | SquareList { elements = [ binding; bound ]; braceSources = _ } as
                     bracks ->
                     let%map parsedBound = parseType bound
                     and parsedBinding = parseBinding binding in
                     Source.
                       { elem = { binding = parsedBinding; bound = parsedBound }
                       ; source = esexpSource bracks
                       }
                   | param -> MResult.err ("Bad parameter syntax", esexpSource param))
                 ~before:(esexpSource funBinding)
                 ~after:rParenSource
             in
             fun parsedBody ->
               Source.
                 { elem =
                     Expr.Let
                       { param =
                           { elem = { binding = parsedFunBinding; bound = None }
                           ; source = parsedFunBinding.source
                           }
                       ; value =
                           wrapFun
                             { elem =
                                 TermLambda
                                   { params = parsedParams; body = parsedFunctionBody }
                             ; source = esexpSource defineExp
                             }
                       ; body = parsedBody
                       }
                 ; source = letSource
                 }
             (* Match a define for a regular value with type annotation *)
           | binding :: Symbol (":", _) :: bound :: valueHead :: valueTail ->
             let%map parsedValue = parseExprBody (valueHead :: valueTail)
             and parsedBound = parseType bound
             and parsedBinding, wrapValue = parseDefineBinding binding in
             fun parsedBody ->
               Source.
                 { elem =
                     Expr.Let
                       { param =
                           { elem = { binding = parsedBinding; bound = Some parsedBound }
                           ; source = SB.merge parsedBinding.source parsedBound.source
                           }
                       ; value = wrapValue parsedValue
                       ; body = parsedBody
                       }
                 ; source = letSource
                 }
             (* Match a define for a regular value *)
           | binding :: valueHead :: valueTail ->
             let%map parsedValue = parseExprBody (valueHead :: valueTail)
             and parsedBinding, wrapValue = parseDefineBinding binding in
             fun parsedBody ->
               Source.
                 { elem =
                     Expr.Let
                       { param =
                           { elem = { binding = parsedBinding; bound = None }
                           ; source = parsedBinding.source
                           }
                       ; value = wrapValue parsedValue
                       ; body = parsedBody
                       }
                 ; source = letSource
                 }
           | _ ->
             MResult.err
               ( "Bad `define` syntax"
               , infixListSource components ~before:defineSource ~after:defineRParenSource
               ))
        | _ -> MResult.err ("Expected a `define` statement", esexpSource define)
      in
      parsedDefine parsedBody
  ;;

  module type Parser = sig
    type t

    val parseEsexps : source Esexp.t NeList.t -> t result
    val parseString : string -> t result
    val parseFile : string -> t result

    module Stage :
      CompilerPipeline.Stage
        with type state = CompilerState.state
        with type input = string
        with type output = t
        with type error = (source option, string) Source.annotate
  end

  module MakeParser (Base : sig
      type t

      val parseEsexps : source Esexp.t NeList.t -> t result
      val name : string
    end) =
  struct
    include Base

    let parseBuffer lexbuf =
      try MOk (Parser.prog Lexer.read lexbuf) with
      | Lexer.SyntaxError (msg, source) -> MResult.err (msg, source)
      | Parser.Error ->
        MResult.err
          ("Syntax error", SB.make ~start:lexbuf.lex_curr_p ~finish:lexbuf.lex_curr_p)
    ;;

    let parseString str =
      let lexbuf = Lexing.from_string ~with_positions:true str in
      let%bind esexps = parseBuffer lexbuf in
      parseEsexps esexps
    ;;

    let parseFile filename =
      let channel = In_channel.open_text filename in
      let lexbuf = Lexing.from_channel ~with_positions:true channel in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let result = parseBuffer lexbuf in
      In_channel.close channel;
      let%bind esexps = result in
      parseEsexps esexps
    ;;

    module Stage = struct
      type state = CompilerState.state
      type input = string
      type output = t
      type error = (source option, string) Source.annotate

      let name = Base.name

      let run input =
        CompilerPipeline.S.returnF
          (match parseString input with
           | MOk _ as expr -> expr
           | Errors errs ->
             Errors
               (NeList.map errs ~f:(fun (elem, source) ->
                  Source.{ elem; source = Some source })))
      ;;
    end
  end

  module IndexParser = MakeParser (struct
      type t = source Index.t

      let parseEsexps (esexps : source Esexp.t NeList.t) =
        match esexps with
        | [ esexp ] -> parseIndex esexp
        | extra :: _ ->
          MResult.err ("Expected one esexp, got more than one", esexpSource extra)
      ;;

      let name = "Parse Index"
    end)

  module TypeParser = MakeParser (struct
      type t = source Type.t

      let parseEsexps (esexps : source Esexp.t NeList.t) =
        match esexps with
        | [ esexp ] -> parseType esexp
        | extra :: _ ->
          MResult.err ("Expected one esexp, got more than one", esexpSource extra)
      ;;

      let name = "Parse Type"
    end)

  module ExprParser = MakeParser (struct
      type t = source Expr.t

      let parseEsexps = parseExprBody
      let name = "Parse Expression"
    end)

  include ExprParser
end

module Default = Make (Source.Builder)

module Unit = Make (struct
    type source = unit

    let make ~start:_ ~finish:_ = ()
    let merge () () = ()
    let between () () = ()
    let show () = ""
  end)

module Stage (SB : Source.BuilderT) = struct
  module Parser = Make (SB)

  type state = CompilerState.state
  type input = string
  type output = SB.source Ast.t
  type error = (SB.source option, string) Source.annotate

  let name = "Parse"

  let run input =
    CompilerPipeline.S.returnF
      (match Parser.parseString input with
       | MOk _ as expr -> expr
       | Errors errs ->
         Errors
           (NeList.map errs ~f:(fun (elem, source) ->
              Source.{ elem; source = Some source })))
  ;;
end
