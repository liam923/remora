open! Base
open MResult
open MResult.Let_syntax

module type S = sig
  type source
  type error = string * source
  type 't result = ('t, error) MResult.t

  module type Parser = sig
    type t

    val parseTexps : source Texp.t NeList.t -> t result
    val parseString : string -> t result
    val parseFile : string -> t result
  end

  module IndexParser : Parser with type t = source Ast.Untyped.Index.t
  module TypeParser : Parser with type t = source Ast.Untyped.Type.t
  module ExprParser : Parser with type t = source Ast.Untyped.Expr.t
  include module type of ExprParser
end

module Make (SB : Source.BuilderT) = struct
  type source = SB.source

  module Lexer = TexpLexer.Make (SB)
  module Parser = TexpParser.Make (SB)

  type error = string * SB.source
  type 't result = ('t, error) MResult.t

  let texpSource = Texp.source (module SB)

  let infixNeListSource list =
    SB.merge (texpSource (NeList.hd list)) (texpSource (NeList.last list))
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

  let parseBindingWithImplicitBound texp ~atBound ~noAtBound =
    match texp with
    | Texp.Symbol (id, source) ->
      let bound = if String.is_prefix id ~prefix:"@" then atBound else noAtBound in
      MOk (Source.{ elem = id; source }, Source.{ elem = bound; source }, source)
    | param -> MResult.err ("Expected an identifier", texpSource param)
  ;;

  let parseBinding = function
    | Texp.Symbol (id, source) ->
      if String.is_prefix id ~prefix:"@"
      then MResult.err ("Expected an identifier without an @", source)
      else MOk Source.{ elem = id; source }
    | param -> MResult.err ("Expected an identifier", texpSource param)
  ;;

  let rec parseIndex : 's Texp.t -> ('s Ast.Untyped.Index.t, error) MResult.t =
    let open Ast.Untyped in
    function
    | Integer (i, source) -> MOk { elem = Index.Dimension i; source }
    | Symbol (id, source) -> MOk { elem = Index.Ref id; source }
    (* Match (+ ...) *)
    | List { braceType = Parens; elements = Symbol ("+", _) :: indices; braceSources = _ }
      as addExp ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(texpSource addExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Add indices)
    (* Match (++ ...) *)
    | List
        { braceType = Parens; elements = Symbol ("++", _) :: indices; braceSources = _ }
      as appendExp ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(texpSource appendExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Append indices)
    (* Match (shape ...) *)
    | List
        { braceType = Parens
        ; elements = Symbol ("shape", _) :: indices
        ; braceSources = _
        } as shapeExp ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(texpSource shapeExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Shape indices)
    (* Match [...]
       Note that the de-sugaring is performed when type checking *)
    | List { braceType = Bracks; elements = indices; braceSources = _ } as shapeExp ->
      let%map parsedIndices =
        parseList indices ~f:parseIndex ~source:(texpSource shapeExp)
      in
      Source.map parsedIndices ~f:(fun indices -> Index.Slice indices)
    | index -> MResult.err ("Bad index syntax", texpSource index)

  and parseType : 's Texp.t -> ('s Ast.Untyped.Type.t, error) MResult.t =
    let open Ast.Untyped in
    (* A function used for parsing Forall, Pi, and Sigma *)
    let parseAbstraction
        (type k)
        (components : source Texp.t list)
        ~(atBound : k)
        ~(noAtBound : k)
        ~symbol
        ~symbolSource
        ~rParenSource
      =
      match components with
      | [ (List { braceType = Parens; elements = parameters; braceSources = _ } as
          paramsExp)
        ; body
        ] ->
        let%map parsedBody = parseType body
        and parsedParameters =
          parseList
            parameters
            ~f:(fun param ->
              let%map binding, bound, source =
                parseBindingWithImplicitBound param ~atBound ~noAtBound
              in
              Source.{ elem = { binding; bound }; source })
            ~source:(texpSource paramsExp)
        in
        Type.{ parameters = parsedParameters; body = parsedBody }
      | _ ->
        MResult.err
          ( [%string "Bad `%{symbol}` syntax"]
          , infixListSource components ~before:symbolSource ~after:rParenSource )
    in
    function
    | Symbol (id, source) -> MOk { elem = Type.Ref id; source }
    | List
        { braceType = Parens
        ; elements = Symbol ("Arr", arrSource) :: components
        ; braceSources = _, rParenSource
        } as arrExp ->
      (match components with
      | [ elementType; shape ] ->
        let%map parsedElementType = parseType elementType
        and parsedShape = parseIndex shape in
        Source.
          { elem = Type.Arr { element = parsedElementType; shape = parsedShape }
          ; source = texpSource arrExp
          }
      | _ ->
        MResult.err
          ( "Bad `Arr` syntax"
          , infixListSource components ~before:arrSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol (arrow, arrSource) :: components
        ; braceSources = _, rParenSource
        } as arrExp
      when String.equal arrow "->" || String.equal arrow "→" ->
      (match components with
      | [ (List { braceType = Parens; elements = parameters; braceSources = _ } as
          paramsExp)
        ; return
        ] ->
        let%map parsedParameters =
          parseList parameters ~f:parseType ~source:(texpSource paramsExp)
        and parsedReturn = parseType return in
        Source.
          { elem = Type.Func { parameters = parsedParameters; return = parsedReturn }
          ; source = texpSource arrExp
          }
      | _ ->
        MResult.err
          ( [%string "Bad `%{arrow}` syntax"]
          , infixListSource components ~before:arrSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol (forall, forallSource) :: components
        ; braceSources = _, rParenSource
        } as forallExp
      when String.equal forall "Forall" || String.equal forall "∀" ->
      let%map abstraction =
        parseAbstraction
          components
          ~atBound:Ast.Kind.Array
          ~noAtBound:Ast.Kind.Atom
          ~symbol:forall
          ~symbolSource:forallSource
          ~rParenSource
      in
      Source.{ elem = Type.Forall abstraction; source = texpSource forallExp }
    | List
        { braceType = Parens
        ; elements = Symbol (pi, piSource) :: components
        ; braceSources = _, rParenSource
        } as piExp
      when String.equal pi "Pi" || String.equal pi "Π" ->
      let%map abstraction =
        parseAbstraction
          components
          ~atBound:Ast.Sort.Shape
          ~noAtBound:Ast.Sort.Dim
          ~symbol:pi
          ~symbolSource:piSource
          ~rParenSource
      in
      Source.{ elem = Type.Pi abstraction; source = texpSource piExp }
    | List
        { braceType = Parens
        ; elements = Symbol (sigma, sigmaSource) :: components
        ; braceSources = _, rParenSource
        } as sigmaExp
      when String.equal sigma "Sigma" || String.equal sigma "Σ" ->
      let%map abstraction =
        parseAbstraction
          components
          ~atBound:Ast.Sort.Shape
          ~noAtBound:Ast.Sort.Dim
          ~symbol:sigma
          ~symbolSource:sigmaSource
          ~rParenSource
      in
      Source.{ elem = Type.Sigma abstraction; source = texpSource sigmaExp }
    | List
        { braceType = Bracks
        ; elements = elementType :: shapeElements
        ; braceSources = _, rBrackSource
        } as arrExp ->
      let%map parsedElementType = parseType elementType
      and parsedShapeElements =
        parseInfixList
          shapeElements
          ~f:parseIndex
          ~before:(texpSource elementType)
          ~after:rBrackSource
      in
      Source.
        { elem =
            Type.Arr
              { element = parsedElementType
              ; shape = Source.map parsedShapeElements ~f:(fun elems -> Index.Slice elems)
              }
        ; source = texpSource arrExp
        }
    | List
        { braceType = Parens
        ; elements = Symbol ("Tuple", _) :: elements
        ; braceSources = _
        } as tupExp ->
      let%map parsedElements =
        parseList elements ~f:parseType ~source:(texpSource tupExp)
      in
      Source.map parsedElements ~f:(fun elements -> Type.Tuple elements)
    | type' -> MResult.err ("Bad type syntax", texpSource type')

  and parseExpr : 's Texp.t -> ('s Ast.Untyped.Expr.t, error) MResult.t =
    let open Ast.Untyped in
    (* array and frame syntax is very similar and both are complex,
       so this abstracts out the commonality *)
    let parseArrayOrFrame
        (components : source Texp.t list)
        ~arrOrFrameExp
        ~symbol
        ~symbolSource
        ~rParenSource
      =
      match components with
      | (List { braceType = Bracks; elements = dimensions; braceSources = _ } as dimsExp)
        :: elements ->
        let%bind parsedDimensions =
          parseList
            dimensions
            ~f:(function
              | Integer (dim, dimSource) -> MOk Source.{ elem = dim; source = dimSource }
              | _ -> Error.raise (Error.of_string "unimplemented"))
            ~source:(texpSource dimsExp)
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
              ; source = texpSource arrOrFrameExp
              }
          | None ->
            MResult.err
              ( [%string "Bad `%{symbol}` syntax - not enough elements"]
              , SB.between (texpSource dimsExp) rParenSource ))
        | Some _ ->
          (match elements with
          | [ elementType ] ->
            let%map parsedElementType = parseType elementType in
            Source.
              { elem =
                  `Empty
                    Expr.
                      { elementType = parsedElementType; dimensions = parsedDimensions }
              ; source = texpSource arrOrFrameExp
              }
          | _ ->
            MResult.err
              ( [%string "Bad `%{symbol}` syntax - expected element type"]
              , infixListSource elements ~before:(texpSource dimsExp) ~after:rParenSource
              )))
      | _ ->
        MResult.err
          ( [%string "Bad `%{symbol}` syntax"]
          , infixListSource components ~before:symbolSource ~after:rParenSource )
    in
    function
    | Symbol (id, source) -> MOk { elem = Expr.Ref id; source }
    | Integer (i, source) -> MOk { elem = Expr.IntLiteral i; source }
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
    | List { braceType = Bracks; elements; braceSources = _ } as frameExp ->
      let source = texpSource frameExp in
      (match elements with
      | head :: tail ->
        let%map parsedElements =
          parseNeList (head :: tail) ~f:parseExpr ~source:(texpSource frameExp)
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
    | List
        { braceType = Parens
        ; elements = Symbol ("array", arrSource) :: components
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
    | List
        { braceType = Parens
        ; elements = Symbol ("frame", frameSource) :: components
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
    | List
        { braceType = Parens
        ; elements = Symbol ("t-app", tAppSource) :: components
        ; braceSources = _, rParenSource
        } as tAppExpr ->
      (match components with
      | tFunc :: args ->
        let%map tFuncParsed = parseExpr tFunc
        and argsParsed =
          parseInfixList args ~f:parseType ~before:(texpSource tFunc) ~after:rParenSource
        in
        Source.
          { elem = Expr.TypeApplication { tFunc = tFuncParsed; args = argsParsed }
          ; source = texpSource tAppExpr
          }
      | _ ->
        MResult.err
          ( "Bad `t-app` syntax"
          , infixListSource components ~before:tAppSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol ("i-app", iAppSource) :: components
        ; braceSources = _, rParenSource
        } as iAppExpr ->
      (match components with
      | iFunc :: args ->
        let%map iFuncParsed = parseExpr iFunc
        and argsParsed =
          parseInfixList args ~f:parseIndex ~before:(texpSource iFunc) ~after:rParenSource
        in
        Source.
          { elem = Expr.IndexApplication { iFunc = iFuncParsed; args = argsParsed }
          ; source = texpSource iAppExpr
          }
      | _ ->
        MResult.err
          ( "Bad `i-app` syntax"
          , infixListSource components ~before:iAppSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol ("unbox", unboxSource) :: components
        ; braceSources = _, rParenSource
        } as unboxExpr ->
      (match components with
      | box
        :: List
             { braceType = Parens
             ; elements = valueBinding :: indexBindings
             ; braceSources = _, bindingsRParen
             }
        :: bodyHead
        :: bodyTail ->
        let parseIndexBinding indexBinding =
          let%map binding, bound, source =
            parseBindingWithImplicitBound
              indexBinding
              ~atBound:Ast.Sort.Shape
              ~noAtBound:Ast.Sort.Dim
          in
          Source.{ elem = Expr.{ binding; bound = Some bound }; source }
        in
        let%map parsedIndexBindings =
          parseInfixList
            indexBindings
            ~f:parseIndexBinding
            ~before:(texpSource valueBinding)
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
          ; source = texpSource unboxExpr
          }
      | _ ->
        MResult.err
          ( "Bad `unbox` syntax"
          , infixListSource components ~before:unboxSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol ("boxes", boxesSource) :: components
        ; braceSources = _, rParenSource
        } as boxesExpr ->
      (match components with
      | (List { braceType = Parens; elements = params; braceSources = _ } as paramsExp)
        :: elementType
        :: (List { braceType = Bracks; elements = dimensions; braceSources = _ } as
           dimsExp)
        :: elements ->
        let parseElement = function
          | Texp.List
              { braceType = Parens
              ; elements =
                  List { braceType = Parens; elements = indices; braceSources = _ }
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
              ; source = texpSource elementExp
              }
          | elementExp ->
            MResult.err
              ("Bad `boxes` syntax - expected a box element", texpSource elementExp)
        in
        let parseDimension = function
          | Texp.Integer (i, source) -> MOk Source.{ elem = i; source }
          | dim -> MResult.err ("Expected an integer", texpSource dim)
        in
        let%map parsedParams =
          parseList
            params
            ~f:(fun param ->
              let%map binding, bound, source =
                parseBindingWithImplicitBound
                  param
                  ~atBound:Ast.Sort.Shape
                  ~noAtBound:Ast.Sort.Dim
              in
              Source.{ elem = { binding; bound }; source })
            ~source:(texpSource paramsExp)
        and parsedElementType = parseType elementType
        and parsedDimensions =
          parseList dimensions ~f:parseDimension ~source:(texpSource dimsExp)
        and parsedElements =
          parseInfixList
            elements
            ~f:parseElement
            ~before:(texpSource dimsExp)
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
          ; source = texpSource boxesExpr
          }
      | _ ->
        MResult.err
          ( "Bad `boxes` syntax"
          , infixListSource components ~before:boxesSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol ("box", boxSource) :: components
        ; braceSources = _, rParenSource
        } as boxExpr ->
      (match components with
      | (List { braceType = Parens; elements = args; braceSources = _ } as argsExp)
        :: elementType
        :: bodyHead
        :: bodyTail ->
        let parseArg = function
          | Texp.List
              { braceType = Parens; elements = [ param; index ]; braceSources = _ } ->
            let%map paramBinding, paramBound, paramSource =
              parseBindingWithImplicitBound
                param
                ~atBound:Ast.Sort.Shape
                ~noAtBound:Ast.Sort.Dim
            and parsedIndex = parseIndex index in
            ( Source.
                { elem = { binding = paramBinding; bound = paramBound }
                ; source = paramSource
                }
            , parsedIndex )
          | argExp ->
            MResult.err ("Bad `box` syntax - expected an arguement", texpSource argExp)
        in
        let%map parsedParamsAndIndices =
          parseList args ~f:parseArg ~source:(texpSource argsExp)
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
                ; dimensions = { elem = []; source = texpSource boxExpr }
                ; elements =
                    { elem =
                        [ { elem = { indices = parsedIndices; body = parsedBody }
                          ; source = texpSource boxExpr
                          }
                        ]
                    ; source = texpSource boxExpr
                    }
                }
          ; source = texpSource boxExpr
          }
      | _ ->
        MResult.err
          ( "Bad `box` syntax"
          , infixListSource components ~before:boxSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol ("tuple", tupleSource) :: elements
        ; braceSources = _, rParenSource
        } as tupleExpr ->
      let%map parsedElements =
        parseInfixList elements ~f:parseExpr ~before:tupleSource ~after:rParenSource
      in
      Source.{ elem = Expr.Tuple parsedElements; source = texpSource tupleExpr }
    | List
        { braceType = Parens
        ; elements = Symbol ("let", letSource) :: components
        ; braceSources = _, rParenSource
        } as letExpr ->
      (match components with
      | (List { braceType = Bracks; elements = declarationComponents; braceSources = _ }
        as decExp)
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
          | _ -> MResult.err ("Bad `let` syntax - bad declaration", texpSource decExp)
        in
        Source.
          { elem =
              Expr.Let { param = parsedParam; value = parsedValue; body = parsedBody }
          ; source = texpSource letExpr
          }
      | _ ->
        MResult.err
          ( "Bad `let` syntax"
          , infixListSource components ~before:letSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol ("let-tuple", letSource) :: components
        ; braceSources = _, rParenSource
        } as letExpr ->
      (match components with
      | List
          { braceType = Bracks
          ; elements = declarationComponents
          ; braceSources = decLBrack, decRBrack
          }
        :: bodyHead
        :: bodyTail ->
        (match declarationComponents with
        | [ List { braceType = Parens; elements = params; braceSources = _ }; value ] ->
          let parseParam = function
            | Texp.List
                { braceType = Parens
                ; elements = [ binding; Symbol (":", _); bound ]
                ; braceSources = _
                } as paramExp ->
              let%map parsedBinding = parseBinding binding
              and parsedBound = parseType bound in
              Source.
                { elem = { binding = parsedBinding; bound = Some parsedBound }
                ; source = texpSource paramExp
                }
            | binding ->
              let%map parsedBinding = parseBinding binding in
              Source.
                { elem = { binding = parsedBinding; bound = None }
                ; source = parsedBinding.source
                }
          in
          let%map parsedValue = parseExpr value
          and parsedParams =
            parseInfixList
              params
              ~before:decLBrack
              ~after:(texpSource value)
              ~f:parseParam
          and parsedBody = parseExprBody (bodyHead :: bodyTail) in
          Source.
            { elem =
                Expr.TupleLet
                  { params = parsedParams; value = parsedValue; body = parsedBody }
            ; source = texpSource letExpr
            }
        | _ ->
          MResult.err
            ( "Bad `let-tuple` syntax - bad declaration"
            , infixListSource declarationComponents ~before:decLBrack ~after:decRBrack ))
      | _ ->
        MResult.err
          ( "Bad `let-tuple` syntax"
          , infixListSource components ~before:letSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol (lambda, lambdaSource) :: components
        ; braceSources = _, rParenSource
        } as lambdaExp
      when String.equal lambda "λ" || String.equal lambda "fn" ->
      (match components with
      | (List { braceType = Parens; elements = params; braceSources = _ } as paramsExp)
        :: bodyHead
        :: bodyTail ->
        let parseParam = function
          | Texp.List
              { braceType = Bracks; elements = [ binding; bound ]; braceSources = _ } as
            paramExp ->
            let%map parsedBinding = parseBinding binding
            and parsedBound = parseType bound in
            Source.
              { elem = { binding = parsedBinding; bound = parsedBound }
              ; source = texpSource paramExp
              }
          | paramExp ->
            MResult.err
              ([%string "Bad `%{lambda}` syntax - bad parameter"], texpSource paramExp)
        in
        let%map parsedParams =
          parseList params ~f:parseParam ~source:(texpSource paramsExp)
        and parsedBody = parseExprBody (bodyHead :: bodyTail) in
        Source.
          { elem = Expr.TermLambda { params = parsedParams; body = parsedBody }
          ; source = texpSource lambdaExp
          }
      | _ ->
        MResult.err
          ( [%string "Bad `%{lambda}` syntax"]
          , infixListSource components ~before:lambdaSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol (tlambda, tlambdaSource) :: components
        ; braceSources = _, rParenSource
        } as tlambdaExp
      when String.equal tlambda "Tλ" || String.equal tlambda "t-fn" ->
      (match components with
      | (List { braceType = Parens; elements = params; braceSources = _ } as paramsExp)
        :: bodyHead
        :: bodyTail ->
        let parseParam param =
          let%map binding, bound, source =
            parseBindingWithImplicitBound
              param
              ~atBound:Ast.Kind.Array
              ~noAtBound:Ast.Kind.Atom
          in
          Source.{ elem = { binding; bound }; source }
        in
        let%map parsedParams =
          parseList params ~f:parseParam ~source:(texpSource paramsExp)
        and parsedBody = parseExprBody (bodyHead :: bodyTail) in
        Source.
          { elem = Expr.TypeLambda { params = parsedParams; body = parsedBody }
          ; source = texpSource tlambdaExp
          }
      | _ ->
        MResult.err
          ( [%string "Bad `%{tlambda}` syntax"]
          , infixListSource components ~before:tlambdaSource ~after:rParenSource ))
    | List
        { braceType = Parens
        ; elements = Symbol (ilambda, ilambdaSource) :: components
        ; braceSources = _, rParenSource
        } as ilambdaExp
      when String.equal ilambda "Iλ" || String.equal ilambda "i-fn" ->
      (match components with
      | (List { braceType = Parens; elements = params; braceSources = _ } as paramsExp)
        :: bodyHead
        :: bodyTail ->
        let parseParam param =
          let%map binding, bound, source =
            parseBindingWithImplicitBound
              param
              ~atBound:Ast.Sort.Shape
              ~noAtBound:Ast.Sort.Dim
          in
          Source.{ elem = { binding; bound }; source }
        in
        let%map parsedParams =
          parseList params ~f:parseParam ~source:(texpSource paramsExp)
        and parsedBody = parseExprBody (bodyHead :: bodyTail) in
        Source.
          { elem = Expr.IndexLambda { params = parsedParams; body = parsedBody }
          ; source = texpSource ilambdaExp
          }
      | _ ->
        MResult.err
          ( [%string "Bad `%{ilambda}` syntax"]
          , infixListSource components ~before:ilambdaSource ~after:rParenSource ))
    | List { braceType = Parens; elements = func :: args; braceSources = _, rParenSource }
      as termAppExp ->
      let%map parsedFunc = parseExpr func
      and parsedArgs =
        parseInfixList args ~f:parseExpr ~before:(texpSource func) ~after:rParenSource
      in
      Source.
        { elem = Expr.TermApplication { func = parsedFunc; args = parsedArgs }
        ; source = texpSource termAppExp
        }
    | expr -> MResult.err ("Bad expression syntax", texpSource expr)

  (* Parse a list of define statements followed by a expression *)
  and parseExprBody
      : source Texp.t NeList.t -> (source Ast.Untyped.Expr.t, error) MResult.t
    =
    let open Ast.Untyped in
    function
    | [ body ] -> parseExpr body
    | define :: next :: rest ->
      let body : source Texp.t NeList.t = next :: rest in
      let letSource = infixNeListSource (define :: next :: rest) in
      let%map parsedBody = parseExprBody body
      and parsedDefine =
        match define with
        | List
            { braceType = Parens
            ; elements = Symbol ("define", defineSource) :: components
            ; braceSources = _, defineRParenSource
            } as defineExp ->
          (match components with
          (* Match a define for a function *)
          | List
              { braceType = Parens
              ; elements = funBinding :: params
              ; braceSources = _, rParenSource
              }
            :: functionBodyHead
            :: functionBodyTail ->
            let%map parsedFunctionBody =
              parseExprBody (functionBodyHead :: functionBodyTail)
            and parsedFunBinding = parseBinding funBinding
            and parsedParams =
              parseInfixList
                params
                ~f:(function
                  | List
                      { braceType = Bracks
                      ; elements = [ binding; bound ]
                      ; braceSources = _
                      } as bracks ->
                    let%map parsedBound = parseType bound
                    and parsedBinding = parseBinding binding in
                    Source.
                      { elem = { binding = parsedBinding; bound = parsedBound }
                      ; source = texpSource bracks
                      }
                  | param -> MResult.err ("Bad parameter syntax", texpSource param))
                ~before:(texpSource funBinding)
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
                          { elem =
                              TermLambda
                                { params = parsedParams; body = parsedFunctionBody }
                          ; source = texpSource defineExp
                          }
                      ; body = parsedBody
                      }
                ; source = letSource
                }
            (* Match a define for a regular value with type annotation *)
          | binding :: Symbol (":", _) :: bound :: valueHead :: valueTail ->
            let%map parsedValue = parseExprBody (valueHead :: valueTail)
            and parsedBound = parseType bound
            and parsedBinding = parseBinding binding in
            fun parsedBody ->
              Source.
                { elem =
                    Expr.Let
                      { param =
                          { elem = { binding = parsedBinding; bound = Some parsedBound }
                          ; source = SB.merge parsedBinding.source parsedBound.source
                          }
                      ; value = parsedValue
                      ; body = parsedBody
                      }
                ; source = letSource
                }
            (* Match a define for a regular value *)
          | binding :: valueHead :: valueTail ->
            let%map parsedValue = parseExprBody (valueHead :: valueTail)
            and parsedBinding = parseBinding binding in
            fun parsedBody ->
              Source.
                { elem =
                    Expr.Let
                      { param =
                          { elem = { binding = parsedBinding; bound = None }
                          ; source = parsedBinding.source
                          }
                      ; value = parsedValue
                      ; body = parsedBody
                      }
                ; source = letSource
                }
          | _ ->
            MResult.err
              ( "Bad `define` syntax"
              , infixListSource components ~before:defineSource ~after:defineRParenSource
              ))
        | _ -> MResult.err ("Expected a `define` statement", texpSource define)
      in
      parsedDefine parsedBody
  ;;

  module type Parser = sig
    type t

    val parseTexps : source Texp.t NeList.t -> t result
    val parseString : string -> t result
    val parseFile : string -> t result
  end

  module MakeParser (Base : sig
    type t

    val parseTexps : source Texp.t NeList.t -> t result
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
      let%bind texps = parseBuffer lexbuf in
      parseTexps texps
    ;;

    let parseFile filename =
      let channel = In_channel.open_text filename in
      let lexbuf = Lexing.from_channel ~with_positions:true channel in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let result = parseBuffer lexbuf in
      In_channel.close channel;
      let%bind texps = result in
      parseTexps texps
    ;;
  end

  module IndexParser = MakeParser (struct
    type t = source Ast.Untyped.Index.t

    let parseTexps (texps : source Texp.t NeList.t) =
      match texps with
      | [ texp ] -> parseIndex texp
      | extra :: _ ->
        MResult.err ("Expected one texp, got more than one", texpSource extra)
    ;;
  end)

  module TypeParser = MakeParser (struct
    type t = source Ast.Untyped.Type.t

    let parseTexps (texps : source Texp.t NeList.t) =
      match texps with
      | [ texp ] -> parseType texp
      | extra :: _ ->
        MResult.err ("Expected one texp, got more than one", texpSource extra)
    ;;
  end)

  module ExprParser = MakeParser (struct
    type t = source Ast.Untyped.Expr.t

    let parseTexps = parseExprBody
  end)

  include ExprParser
end

module Default = Make (Source.Builder)

module Unit = Make (struct
  type source = unit

  let make ~start:_ ~finish:_ = ()
  let merge () () = ()
  let between () () = ()
end)
