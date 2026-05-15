%{
open Ast
%}

%token <string> IDENT
%token <Ast.var option> VAR // For types and const generics
%token <Ast.var option> REGION
%token <Z.t> INT
%token WILDCARD
%token STATIC_REGION
%token SEP TRUE FALSE
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_CURLY RIGHT_CURLY
%token LEFT_SQUARE RIGHT_SQUARE
%token LEFT_ANGLE RIGHT_ANGLE
%token SEMICOL AMPERSAND MUT CONST COMMA EOF FN ARROW STAR HASH

/* Types */

%type <pattern> full_pattern
%type <pattern> pattern
%type <pattern_elem> pattern_elem
%type <expr> expr
%type <region> region
%type <generic_args> generic_args
%type <generic_arg> generic_arg

/* Entry point */

%start full_pattern

%%

full_pattern:
  | n=pattern EOF { n }

pattern:
  | e=pattern_elem { [e] }
  | e=pattern_elem SEP n=pattern { e :: n }

pattern_elem:
  | WILDCARD { PWild }
  // (Instantiated) identifier
  | id=IDENT { PIdent (id, 0, []) }
  | id=IDENT; HASH; disamb=INT { PIdent (id, Z.to_int disamb, []) }
  | id=IDENT; LEFT_ANGLE; g=generic_args; RIGHT_ANGLE { PIdent (id, 0, g) }
  | id=IDENT; HASH; disamb=INT; LEFT_ANGLE; g=generic_args; RIGHT_ANGLE { PIdent (id, Z.to_int disamb, g) }
  // Impl path elem
  | LEFT_CURLY; ty=expr; RIGHT_CURLY { PImpl ty }

expr:
  // Compound types - note that if a type has generics, they will be grouped
  // with the last pattern_elem of the name
  | n=pattern { EComp n }
  // Primitive ADT: Tuple
  | LEFT_BRACKET; tys=separated_list(COMMA, expr); RIGHT_BRACKET {
      EPrimAdt (TTuple, List.map (fun x -> GExpr x) tys) }
  // Primitive ADT: Slice
  | LEFT_SQUARE; ty=expr; RIGHT_SQUARE {
      EPrimAdt (TSlice, [GExpr ty]) }
  // Primitive ADT: Array
  | LEFT_SQUARE; ty=expr; SEMICOL; cg=expr; RIGHT_SQUARE {
      EPrimAdt (TArray, [GExpr ty; GExpr cg]) }
  | LEFT_SQUARE; ty=expr; SEMICOL; cg=INT; RIGHT_SQUARE {
      EPrimAdt (TArray, [GExpr ty; GValue (LInt cg)]) }
  // References
  | AMPERSAND; r=region; MUT; ty=expr {
      ERef (r, ty, RMut) }
  | AMPERSAND; r=region; ty=expr {
      ERef (r, ty, RShared) }
  // Variables
  | v=VAR { EVar v }
  // Arrows
  | FN; LEFT_BRACKET; inputs=separated_list(COMMA, expr); RIGHT_BRACKET; ARROW; ret=expr {
    EArrow (inputs, Some ret) }
  | FN; LEFT_BRACKET; inputs=separated_list(COMMA, expr); RIGHT_BRACKET {
    EArrow (inputs, None) }
  // Raw pointers
  | STAR; MUT; ty=expr {
    ERawPtr (Mut, ty) }
  | STAR; CONST; ty=expr {
    ERawPtr (Not, ty) }
  ;

region:
  | STATIC_REGION { RStatic }
  | r=REGION { RVar r }

generic_args:
  | g=generic_arg { [ g ] }
  | g=generic_arg; COMMA; gl=generic_args { g :: gl }

generic_arg:
  // Expressions
  | e=expr { GExpr e }
  // Values
  | TRUE { GValue (LBool true) }
  | FALSE { GValue (LBool false) }
  | v=INT { GValue (LInt v) }
  // Regions
  | r=region { GRegion r }
