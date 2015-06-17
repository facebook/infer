/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

/* Parser for types in the ast of clang */
%{

%}

%token INLINE STATIC CONST STARCONST EXTERN VOID CHAR SHORT INT LONG FLOAT DOUBLE UND_BOOL VOLATILE STARVOLATILE CARRET
%token CLASS STRUCT UNION UND_UND_UINT16_ UND_UND_UINT16_T UND_UND_UINT32_ UND_UND_INT32_T UND_UND_UINT32_T
%token UND_UND_INT64_T UND_UND_UINT64_T UINT8 UINT16 UINT32 UINT64 UNSIGNED SIGNED ENUM BUILTIN_FN_TYPE TYPENAME
%token UND_UND_STRONG UND_UND_UNSAFE_RETAIN UND_UND_WEAK UND_UND_AUTORELEASING NOEXCEPT
%token STAR_UND_UND_STRONG STAR_UND_UND_UNSAFE_UNRETAINED UND_UND_UNSAFE_UNRETAINED STAR_UND_UND_WEAK STAR_UND_UND_AUTORELEASING

%token DOT SEMICOLON COLON COMMA SINGLE_QUOTE DOUBLE_QUOTE REV_QUOTE
%token PERCENT AMPERSAND EXCLAMATION EQUAL MINUS PLUS RESTRICT
%token LEFT_CHEVRON RIGHT_CHEVRON LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_SQBRACKET RIGHT_SQBRACKET LEFT_BRACE RIGHT_BRACE
%token STAR PIPE SLASH BACKSLASH

%token <string> HEXA
%token <string> NUMBER
%token <string> IDENT
%token <string> ANONYM_IDENT
%token <string> NESTED_IDENT
%token <string> NESTED_ANONYM_IDENT

%token EOF

%start parse
%start pointer_clang_type
%start clang_func_type

%type <Sil.typ> pointer_clang_type
%type <Sil.typ> parse
%type <(Sil.typ * Sil.typ list)> clang_func_type

%%

keyword:
  | ENUM { "enum" }
  | UNSIGNED { "unsigned" }
  | SIGNED { "signed" }
  | CLASS { "class" }
  | STRUCT { "struct" }
  | UNION { "union" }
  | VOID { "void" }
  | CHAR { "char" }
  | SHORT { "short" }
  | INT { "int" }
  | LONG { "long" }
  | TYPENAME { "typename" }
  | NOEXCEPT { "noexcept" }
  | CONST { "const" }
  | STARCONST { "*const" }
  | FLOAT { "float" }
  | DOUBLE { "double" }
  | VOLATILE { "volatile" }
  | RESTRICT { "restrict" }
  | STARVOLATILE { "*volatile" }


ident:
  | IDENT { $1 }
;

anonym_ident:
  | ANONYM_IDENT { $1 }
;

nested_ident:
  | NESTED_IDENT { $1 }
;

nested_anonym_ident:
  | NESTED_ANONYM_IDENT { $1 }
;

ident_csu:
  | ident {$1}
  | anonym_ident { $1 }
  | nested_ident { $1 }
  | nested_anonym_ident { $1 }

identk:
  | ident { $1 }
  | keyword { $1 }
;

csu_sil:
  | CLASS  { Sil.Class }
  | STRUCT { Sil.Struct }
  | UNION  { Sil.Union }
;

arc_qualifier:
  | UND_UND_STRONG { Sil.Pk_pointer }
  | UND_UND_UNSAFE_UNRETAINED { Sil.Pk_objc_unsafe_unretained }
  | UND_UND_WEAK { Sil.Pk_objc_weak }
  | UND_UND_AUTORELEASING { Sil.Pk_objc_autoreleasing }
;

star_arc_qualifier:
  | STAR_UND_UND_STRONG { Sil.Pk_pointer }
  | STAR_UND_UND_UNSAFE_UNRETAINED { Sil.Pk_objc_unsafe_unretained }
  | STAR_UND_UND_WEAK { Sil.Pk_objc_weak }
  | STAR_UND_UND_AUTORELEASING { Sil.Pk_objc_autoreleasing }
;

number_list:
  | { [] }
  | NUMBER { [$1] } /* For dealing with an unspecified number of arguments */
  | NUMBER COMMA number_list { $1::$3 }
;

pointer_clang_type_list:
  | { [] }
  | DOT DOT DOT { [] } /* For dealing with an unspecified number of arguments */
  | pointer_clang_type { [$1]}
  | pointer_clang_type COMMA pointer_clang_type_list { $1::$3 }
;

array_index:
  | LEFT_SQBRACKET NUMBER RIGHT_SQBRACKET { Sil.Const (Sil.Cint((Sil.int_of_int64_kind (Int64.of_string $2) (Sil.IInt)))) }
;

block_type:
  | pointer_clang_type LEFT_PARENTHESIS CARRET RIGHT_PARENTHESIS
      LEFT_PARENTHESIS pointer_clang_type_list RIGHT_PARENTHESIS
                      { Sil.Tfun false }
  | pointer_clang_type LEFT_PARENTHESIS CARRET arc_qualifier RIGHT_PARENTHESIS
      LEFT_PARENTHESIS pointer_clang_type_list RIGHT_PARENTHESIS
                      { Sil.Tfun false }
  | pointer_clang_type LEFT_PARENTHESIS CARRET CONST arc_qualifier RIGHT_PARENTHESIS
      LEFT_PARENTHESIS pointer_clang_type_list RIGHT_PARENTHESIS
                      { Sil.Tfun false }
  | pointer_clang_type LEFT_PARENTHESIS CARRET CONST RIGHT_PARENTHESIS
      LEFT_PARENTHESIS pointer_clang_type_list RIGHT_PARENTHESIS
                      { Sil.Tfun false }
;

array_qualifier:
  | CONST {}
  | arc_qualifier {} /* Currently array do not deal with arc qualifiers */
;

template:
  | LEFT_CHEVRON pointer_clang_type_list RIGHT_CHEVRON {}
  | LEFT_CHEVRON number_list RIGHT_CHEVRON {}
;

/* Nested arrays must be translated inside-out. */
array_indices:
  | array_index { [$1] }
  | array_index array_indices { $1 :: $2 }

pointer_clang_type:
  | pointer_clang_type array_indices { List.fold_left (fun t x -> Sil.Tarray (t, x)) $1 (List.rev $2) }
  | pointer_clang_type LEFT_SQBRACKET RIGHT_SQBRACKET { Sil.Tptr($1, Sil.Pk_pointer) }
  | pointer_clang_type STAR { Sil.Tptr($1, Sil.Pk_pointer) }
  | pointer_clang_type AMPERSAND { Sil.Tptr($1, Sil.Pk_reference) }
  | pointer_clang_type STARCONST arc_qualifier { Sil.Tptr($1, $3) }
  | arc_qualifier pointer_clang_type { Sil.Tptr($2,$1) }
  | pointer_clang_type STARCONST { Sil.Tptr($1, Sil.Pk_pointer) }
  | pointer_clang_type STARVOLATILE { Sil.Tptr($1, Sil.Pk_pointer) }
  | pointer_clang_type STAR RESTRICT { Sil.Tptr($1, Sil.Pk_pointer) }
  | pointer_clang_type star_arc_qualifier { Sil.Tptr($1, $2) }
  | clang_type { $1 }
  | clang_type array_qualifier array_indices { List.fold_left (fun t x -> Sil.Tarray (t, x)) $1 (List.rev $3) }
  | clang_type template { $1 }
  | pointer_clang_type LEFT_PARENTHESIS STAR RIGHT_PARENTHESIS
                       LEFT_PARENTHESIS pointer_clang_type_list RIGHT_PARENTHESIS
                      { Sil.Tptr(Sil.Tfun false, Sil.Pk_pointer) }
  | pointer_clang_type LEFT_PARENTHESIS STAR RIGHT_PARENTHESIS
                       LEFT_PARENTHESIS pointer_clang_type_list RIGHT_PARENTHESIS NOEXCEPT
                      { Sil.Tptr(Sil.Tfun false, Sil.Pk_pointer) }
  | block_type { Sil.Tptr($1, Sil.Pk_pointer)  }
  | pointer_clang_type LEFT_PARENTHESIS pointer_clang_type_list RIGHT_PARENTHESIS
                { CFrontend_utils.Printing.log_err "WARNING: PARSING with TFun!\n";
                  Sil.Tfun false }
;

clang_type:
  | UND_UND_UINT16_ { Sil.Tint Sil.IUInt }
  | UND_UND_UINT16_T { Sil.Tint Sil.IUInt }
  | UND_UND_UINT32_ { Sil.Tint Sil.IUInt }
  | UND_UND_UINT32_T { Sil.Tint Sil.IUInt }
  | UND_UND_UINT64_T { Sil.Tint Sil.IUInt }
  | UND_UND_INT64_T { Sil.Tint Sil.IInt }
  | UND_UND_INT32_T { Sil.Tint Sil.IInt }
  | UINT8   { Sil.Tint Sil.IUInt }
  | UINT16  { Sil.Tint Sil.IUInt }
  | UINT32  { Sil.Tint Sil.IUInt }
  | UINT64  { Sil.Tint Sil.IUInt }
  | UNSIGNED INT { Sil.Tint Sil.IUInt }
  | UNSIGNED LONG LONG { Sil.Tint Sil.IULongLong }
  | UNSIGNED LONG  { Sil.Tint Sil.IULong }
  | UNSIGNED SHORT  { Sil.Tint Sil.IUShort }
  | FLOAT { Sil.Tfloat Sil.FFloat }
  | DOUBLE { Sil.Tfloat Sil.FDouble }
  | VOID   { Sil.Tvoid }
  | CHAR   { Sil.Tint Sil.IChar }
  | SIGNED CHAR { Sil.Tint Sil.ISChar }
  | UNSIGNED CHAR  { Sil.Tint Sil.IUChar }
  | INT    { Sil.Tint Sil.IInt }
  | UND_BOOL  { Sil.Tint Sil.IBool }
  | SHORT  { Sil.Tint Sil.IShort }
  | LONG   { Sil.Tint Sil.ILong }
  | LONG LONG { Sil.Tint Sil.ILongLong }
  | LONG DOUBLE { Sil.Tfloat Sil.FLongDouble }
  | ENUM ident {Sil.Tvar (Sil.TN_enum (Mangled.from_string ("enum "^$2))) }
  | BUILTIN_FN_TYPE { CFrontend_utils.Printing.log_err "WARNING: Parsing this with Tfun!\n";
                      Sil.Tfun false }
  | CONST pointer_clang_type { $2 }
  | CONST TYPENAME pointer_clang_type { $3 }
  | VOLATILE pointer_clang_type { $2 }
  | ident ANONYM_IDENT { CFrontend_utils.Printing.log_out "   ...Found just an identifier modified with a protocol. Ignoring protocol!. Parsing as Named Type!\n";
            Sil.Tvar (Sil.TN_typedef(Mangled.from_string $1))}
  | ident { CFrontend_utils.Printing.log_out "   ...Found just an identifier. Parsing as Named Type %s !\n" $1;
            Sil.Tvar (Sil.TN_typedef(Mangled.from_string $1))}
  | csu_sil ident_csu { let typename=Sil.TN_csu($1, Mangled.from_string $2) in
                        Sil.Tvar typename }
;

clang_type_list:
  | clang_type COMMA clang_type_list {$1 :: $3}
  | clang_type {[$1]}

clang_func_type:
  |  clang_type LEFT_PARENTHESIS clang_type_list RIGHT_PARENTHESIS
    {($1, $3)}


parse:
  | pointer_clang_type { $1 }
