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
  | left = tokenSource(LEFT_PAREN); texpList = texp*; right = tokenSource(RIGHT_PAREN) { Texp.List {braceType=Parens; elements=texpList; braceSources=(left, right)}  }
  | left = tokenSource(LEFT_BRACK); texpList = texp*; right = tokenSource(RIGHT_BRACK) { Texp.List {braceType=Bracks; elements=texpList; braceSources=(left, right)}  }
  | s = STRING                                                                         { Texp.String  (s,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | i = INT                                                                            { Texp.Integer (i,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | s = SYMBOL                                                                         { Texp.Symbol  (s,        SourceBuilder.make ~start:$startpos ~finish:$endpos) } ;
    
tokenSource(TOKEN) : TOKEN { SourceBuilder.make ~start:$startpos ~finish:$endpos }
