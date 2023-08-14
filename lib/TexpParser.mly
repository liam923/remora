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
%start <SourceBuilder.source Texp.t NeList.t> prog
%%

prog:
  | head = texp; rest = texp*; EOF { NeList.(head :: rest) }

texp:
  | left = tokenSource(LEFT_PAREN); elements = texp*; right = tokenSource(RIGHT_PAREN)   { Texp.List {braceType=Paren; elements; braceSources=(left, right)}  }
  | left = tokenSource(LEFT_SQUARE); elements = texp*; right = tokenSource(RIGHT_SQUARE) { Texp.List {braceType=Square; elements; braceSources=(left, right)}  }
  | base = texp; left = tokenSource(LEFT_CURLY); leftElements = texp*;
    bar = tokenSource(BAR); rightElements = texp*; right = tokenSource(RIGHT_CURLY)      { Texp.WithCurlies { base; leftElements; rightElements; curlySources=(left, right); splitSource=bar } }
  | s = STRING                                                                           { Texp.String  (s,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | i = INT                                                                              { Texp.Integer (i,        SourceBuilder.make ~start:$startpos ~finish:$endpos) }
  | s = SYMBOL                                                                           { Texp.Symbol  (s,        SourceBuilder.make ~start:$startpos ~finish:$endpos) } ;
    
tokenSource(TOKEN) : TOKEN { SourceBuilder.make ~start:$startpos ~finish:$endpos }
