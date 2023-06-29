%parameter <SourceBuilder : Source.BuilderT>

%token <int> INT
%token <string> STRING
%token <string> SYMBOL
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token EOF
%start <SourceBuilder.source Texp.t Non_empty_list.t> prog
%%

prog:
  | head = texp; rest = texp*; EOF { Non_empty_list.(head :: rest) }

texp:
  | LEFT_PAREN; texpList = texp*; RIGHT_PAREN { Texp.ParensList (texpList, SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | LEFT_BRACK; texpList = texp*; RIGHT_BRACK { Texp.BracksList (texpList, SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | s = STRING                                { Texp.String     (s,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | i = INT                                   { Texp.Integer    (i,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | i = SYMBOL                                { Texp.Identifier (i,        SourceBuilder.make ~start:$startpos ~finish:$endpos) } ;
    