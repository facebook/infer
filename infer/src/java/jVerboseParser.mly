%{

%}

%token EOL EOF
%token LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET
%token COLON COMMA
%token PARSING STARTED WROTE SEARCH_PATH
%token REGULARFILEOBJECT ZIPFILEINDEXFILEOBJECT
%token <string> CLASS_FILENAME
%token <string> SOURCE_FILENAME
%token <string> PATH
%start line
%type <JVerbose.parsed_data> line
%%


line:
   | source_filename { $1 }
   | class_filename { $1 }
   | classpath { $1 }
;

source_filename:
  LEFT_SQUARE_BRACKET PARSING STARTED REGULARFILEOBJECT LEFT_SQUARE_BRACKET SOURCE_FILENAME RIGHT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET { JVerbose.Source $6 }
;

class_filename:
  LEFT_SQUARE_BRACKET WROTE REGULARFILEOBJECT LEFT_SQUARE_BRACKET CLASS_FILENAME RIGHT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET { JVerbose.Class $5 }
;

classpath:
  LEFT_SQUARE_BRACKET SEARCH_PATH COLON classpath_parts RIGHT_SQUARE_BRACKET { JVerbose.Classpath $4 }
;

classpath_parts:
  | PATH  { [$1] }
  | PATH COMMA classpath_parts  { $1 :: $3 }
;
