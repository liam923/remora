%parameter <SourceBuilder : Source.BuilderT>

%token <int> INT
%token <string> STRING
%token <string> SYMBOL
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_SQUARE
%token RIGHT_SQUARE
%token LEFT_CURLY
%token RIGHT_CURLY
%token BAR
%token EOF
%start <SourceBuilder.source Esexp.t NeList.t> prog
%%

prog:
  | head = esexp; rest = esexp*; EOF { NeList.(head :: rest) }

esexp:
  | left = tokenSource(LEFT_PAREN); elements = esexp*; right = tokenSource(RIGHT_PAREN)   { Esexp.ParenList {elements; braceSources=(left, right)}  }
  | left = tokenSource(LEFT_SQUARE); elements = esexp*; right = tokenSource(RIGHT_SQUARE) { Esexp.SquareList {elements; braceSources=(left, right)}  }
  | base = esexp; left = tokenSource(LEFT_CURLY); leftElements = esexp*;
    bar = tokenSource(BAR); rightElements = esexp*; right = tokenSource(RIGHT_CURLY)      { Esexp.WithCurlies { base; leftElements; rightElements; curlySources=(left, right); splitSource=bar } }
  | s = STRING                                                                           { Esexp.String  (s,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | i = INT                                                                              { Esexp.Integer (i,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | s = SYMBOL                                                                           { Esexp.Symbol  (s,        SourceBuilder.make ~start:$startpos ~finish:$endpos) } ;
    
tokenSource(TOKEN) : TOKEN { SourceBuilder.make ~start:$startpos ~finish:$endpos }
