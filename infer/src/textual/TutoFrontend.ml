(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ============================================================================================== *)
(*  TUTORIAL: adding a new language frontend to Infer by targeting Textual                        *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  This single file is a *literate-programming* tutorial. It is real, compiling OCaml            *)
(*  (it is part of the [Textuallib] library, exactly like [Textual.ml] next to it), and           *)
(*  at the same time it reads top-to-bottom as a guided tour: each section starts with a          *)
(*  prose comment explaining *what* and *why*, immediately followed by the code that              *)
(*  does it.                                                                                      *)
(*                                                                                                *)
(*  GOAL                                                                                          *)
(*  --------------------------------------------------------------------------------------------- *)
(*  We invent a tiny imperative toy language -- "ToyLang" -- and build a *complete*               *)
(*  frontend for it: from source text all the way to Infer capturing SIL that Pulse can           *)
(*  analyse. The payoff is that Pulse will report a real NULL_DEREFERENCE on a ToyLang            *)
(*  program, proving the frontend works end to end.                                               *)
(*                                                                                                *)
(*  WHY GO THROUGH TEXTUAL?                                                                       *)
(*  --------------------------------------------------------------------------------------------- *)
(*  Infer's internal IR (SIL: [Cfg.t] + [Tenv.t], see infer/src/IR/) is powerful but has          *)
(*  many invariants that are painful to establish by hand (SSA temporaries, pruned                *)
(*  branches, typed locals, ...). Textual is a *higher-level* intermediate language that          *)
(*  sits between your source language and SIL. You produce a [Textual.Module.t] -- which          *)
(*  is forgiving (you may nest side-effecting calls inside expressions, write structured          *)
(*  [if] terminators, etc.) -- and Infer does the hard part for you:                              *)
(*                                                                                                *)
(*      ToyLang source                                                                            *)
(*          |  (1) YOUR parser            -- this file: tokenize + parse                          *)
(*          v                                                                                     *)
(*      ToyLang AST                                                                               *)
(*          |  (2) YOUR translation       -- this file: build_module / compile_*                  *)
(*          v                                                                                     *)
(*      Textual.Module.t                                                                          *)
(*          |  (3) TextualVerification.verify_strict       -- full type check + inference         *)
(*          |  (4) TextualTransform.run_exn                -- remove_if_terminator,               *)
(*          |                                                 remove_effects_in_subexprs,         *)
(*          |                                                 let_propagation, out_of_ssa         *)
(*          |  (5) TextualSil.module_to_sil                -- Textual -> (Cfg.t, Tenv.t)          *)
(*          v                                                                                     *)
(*      SIL (Cfg.t + Tenv.t)                                                                      *)
(*          |  (6) TextualParser.TextualFile.capture       -- store into capture.db               *)
(*          v                                                                                     *)
(*      analysed by Pulse & friends                                                               *)
(*                                                                                                *)
(*  Steps (3)-(6) are reused verbatim from existing frontends (see                                *)
(*  infer/src/integration/Python.ml and Hack.ml). The only language-specific work is              *)
(*  steps (1) and (2). Everything below is organised that way.                                    *)
(*                                                                                                *)
(*  TWO WAYS IN.                                                                                  *)
(*  This tutorial builds the [Textual.Module.t] *in process* (in OCaml), like Python's            *)
(*  frontend. A frontend can instead be an *external tool* that emits Textual as *text*           *)
(*  ([.sil]) which Infer parses -- this is what Hack does (its translator is a separate Rust      *)
(*  compiler). Both converge on the same backend (steps 3-6). See the APPENDIX at the very        *)
(*  bottom of this file for that alternative.                                                     *)
(*                                                                                                *)
(*  EXECUTABLE COMPANION.                                                                         *)
(*  This file builds the pipeline; its twin unit/TutoFrontendTest.ml *runs* it on the             *)
(*  example program and pins the output. Throughout this file you will see jump markers           *)
(*       >>> RUN IT: unit/TutoFrontendTest.ml, test "<name>"                                      *)
(*  telling you exactly when to hop over to the test to watch the stage you just read             *)
(*  about actually execute, then hop back -- so the two files read as one linear tutorial.        *)
(* ============================================================================================== *)

open! IStd
module L = Logging

(* ============================================================================================== *)
(*  THE ToyLang LANGUAGE                                                                          *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  ToyLang is a minimal C-like imperative language. Concrete syntax (by example):                *)
(*                                                                                                *)
(*      // returns the larger of a and b                                                          *)
(*      fun max(a, b) {                                                                           *)
(*        if (a < b) { return b; } else { return a; }                                             *)
(*      }                                                                                         *)
(*                                                                                                *)
(*      fun main() {                                                                              *)
(*        var p = alloc();      // p points to a fresh heap cell (non-null)                       *)
(*        *p = max(3, 7);       // function call + store through a pointer                        *)
(*        var q = nil;          // q is the null pointer                                          *)
(*        return *q;            // BUG: dereferences null -> Pulse NULL_DEREFERENCE               *)
(*      }                                                                                         *)
(*                                                                                                *)
(*  Grammar:                                                                                      *)
(*    program := func*                                                                            *)
(*    func    := 'fun' ident '(' params ')' block                                                 *)
(*    block   := '{' stmt* '}'                                                                    *)
(*    stmt    := 'var' ident '=' expr ';'    (* declare + initialise a local *)                   *)
(*             | ident '=' expr ';'          (* assign an existing variable *)                    *)
(*             | '*' unary '=' expr ';'       (* store through a pointer *)                       *)
(*             | 'if' '(' expr ')' block 'else' block                                             *)
(*             | 'while' '(' expr ')' block                                                       *)
(*             | 'return' expr ';'                                                                *)
(*    expr    := add ('<' add)?              (* '<' is the only comparison *)                     *)
(*    add     := mul (('+'|'-') mul)*                                                             *)
(*    mul     := unary (('*'|'/') unary)*                                                         *)
(*    unary   := '*' unary                   (* pointer dereference *)                            *)
(*             | atom                                                                             *)
(*    atom    := int | 'nil' | 'alloc' '(' ')' | ident '(' args ')' | ident | '(' expr ')'        *)
(* ============================================================================================== *)

(* The abstract syntax tree. We keep source line numbers on statements only: that is              *)
(* enough for Infer to point its reports at the right line. Expressions inherit the line          *)
(* of their enclosing statement.                                                                  *)

type binop = Add | Sub | Mul | Div | Lt

type exp =
  | Int of int  (** integer literal *)
  | Var of string  (** read a variable's current value *)
  | Nil  (** the null pointer *)
  | Alloc  (** [alloc()] : allocate a fresh heap cell, returns a non-null pointer *)
  | Deref of exp  (** [*e] : load the value pointed at by [e] *)
  | Binop of binop * exp * exp
  | Call of string * exp list

type stmt = {line: int; desc: stmt_desc}

and stmt_desc =
  | Declare of string * exp  (** [var x = e;] *)
  | Assign of string * exp  (** [x = e;] *)
  | Store of exp * exp  (** [*ptr = e;] -- first component evaluates to the address *)
  | If of exp * block * block
  | While of exp * block
  | Return of exp

and block = stmt list

type func = {name: string; params: string list; body: block; line: int}

type program = func list

(* ============================================================================================== *)
(*  STEP (1a): THE LEXER                                                                          *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  A hand-written tokenizer. This part is deliberately boring: lexing/parsing is *not*           *)
(*  the subject of this tutorial (a real frontend would reuse the source language's own           *)
(*  compiler or an existing parser, as Hack/Python/Rust do). We include it only so the            *)
(*  tutorial is genuinely end-to-end (text in, report out).                                       *)
(*                                                                                                *)
(*  Note the Infer coding conventions in action: polymorphic equality is disabled, so we          *)
(*  derive [equal_token] with [@@deriving equal] and compare characters with [Char.equal].        *)

type token =
  | TInt of int
  | TIdent of string
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TComma
  | TSemi
  | TEq
  | TLt
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TEof
[@@deriving equal]

let tokenize (s : string) : (token * int) list =
  let n = String.length s in
  let i = ref 0 in
  let line = ref 1 in
  let rev_toks = ref [] in
  let push t = rev_toks := (t, !line) :: !rev_toks in
  while !i < n do
    let c = s.[!i] in
    if Char.equal c '\n' then (
      Int.incr line ;
      Int.incr i )
    else if Char.is_whitespace c then Int.incr i
    else if Char.equal c '/' && !i + 1 < n && Char.equal s.[!i + 1] '/' then
      (* a "//" line comment: skip to end of line *)
      while !i < n && not (Char.equal s.[!i] '\n') do
        Int.incr i
      done
    else if Char.is_digit c then (
      let start = !i in
      while !i < n && Char.is_digit s.[!i] do
        Int.incr i
      done ;
      push (TInt (Int.of_string (String.sub s ~pos:start ~len:(!i - start)))) )
    else if Char.is_alpha c || Char.equal c '_' then (
      let start = !i in
      while !i < n && (Char.is_alphanum s.[!i] || Char.equal s.[!i] '_') do
        Int.incr i
      done ;
      push (TIdent (String.sub s ~pos:start ~len:(!i - start))) )
    else
      let single =
        match c with
        | '(' ->
            TLParen
        | ')' ->
            TRParen
        | '{' ->
            TLBrace
        | '}' ->
            TRBrace
        | ',' ->
            TComma
        | ';' ->
            TSemi
        | '=' ->
            TEq
        | '<' ->
            TLt
        | '+' ->
            TPlus
        | '-' ->
            TMinus
        | '*' ->
            TStar
        | '/' ->
            TSlash
        | _ ->
            L.die UserError "ToyLang lexer: unexpected character %C at line %d" c !line
      in
      push single ;
      Int.incr i
  done ;
  List.rev ((TEof, !line) :: !rev_toks)


(* ============================================================================================== *)
(*  STEP (1b): THE PARSER                                                                         *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  A classic recursive-descent parser. Everything is nested inside [parse] so that the           *)
(*  whole mutually-recursive family shares the same token cursor ([toks] / [pos]).                *)

let parse (source : string) : program =
  let toks = Array.of_list (tokenize source) in
  let pos = ref 0 in
  let peek () = fst toks.(!pos) in
  let peek_line () = snd toks.(!pos) in
  let advance () = Int.incr pos in
  let fail msg = L.die UserError "ToyLang parse error at line %d: %s" (peek_line ()) msg in
  let expect tok = if equal_token (peek ()) tok then advance () else fail "unexpected token" in
  let eat_kw kw =
    match peek () with
    | TIdent x when String.equal x kw ->
        advance ()
    | _ ->
        fail (Printf.sprintf "expected keyword '%s'" kw)
  in
  let parse_ident () =
    match peek () with
    | TIdent x ->
        advance () ;
        x
    | _ ->
        fail "expected an identifier"
  in
  (* The expression grammar, encoding operator precedence by stratified non-terminals. *)
  let rec parse_expr () = parse_cmp ()
  and parse_cmp () =
    let l = parse_add () in
    match peek () with
    | TLt ->
        advance () ;
        Binop (Lt, l, parse_add ())
    | _ ->
        l
  and parse_add () =
    let rec loop l =
      match peek () with
      | TPlus ->
          advance () ;
          loop (Binop (Add, l, parse_mul ()))
      | TMinus ->
          advance () ;
          loop (Binop (Sub, l, parse_mul ()))
      | _ ->
          l
    in
    loop (parse_mul ())
  and parse_mul () =
    let rec loop l =
      match peek () with
      | TStar ->
          advance () ;
          loop (Binop (Mul, l, parse_unary ()))
      | TSlash ->
          advance () ;
          loop (Binop (Div, l, parse_unary ()))
      | _ ->
          l
    in
    loop (parse_unary ())
  and parse_unary () =
    match peek () with
    | TStar ->
        advance () ;
        Deref (parse_unary ())
    | _ ->
        parse_atom ()
  and parse_atom () =
    match peek () with
    | TInt v ->
        advance () ;
        Int v
    | TLParen ->
        advance () ;
        let e = parse_expr () in
        expect TRParen ;
        e
    | TIdent "nil" ->
        advance () ;
        Nil
    | TIdent "alloc" ->
        advance () ;
        expect TLParen ;
        expect TRParen ;
        Alloc
    | TIdent name -> (
        advance () ;
        match peek () with
        | TLParen ->
            advance () ;
            let args = parse_args () in
            expect TRParen ;
            Call (name, args)
        | _ ->
            Var name )
    | _ ->
        fail "expected an expression"
  and parse_args () =
    match peek () with
    | TRParen ->
        []
    | _ -> (
        let a = parse_expr () in
        match peek () with
        | TComma ->
            advance () ;
            a :: parse_args ()
        | _ ->
            [a] )
  in
  (* The statement and declaration grammar. *)
  let rec parse_block () =
    expect TLBrace ;
    let rec loop acc =
      match peek () with
      | TRBrace ->
          advance () ;
          List.rev acc
      | _ ->
          loop (parse_stmt () :: acc)
    in
    loop []
  and parse_stmt () =
    let line = peek_line () in
    let desc =
      match peek () with
      | TIdent "var" ->
          advance () ;
          let x = parse_ident () in
          expect TEq ;
          let e = parse_expr () in
          expect TSemi ;
          Declare (x, e)
      | TIdent "return" ->
          advance () ;
          let e = parse_expr () in
          expect TSemi ;
          Return e
      | TIdent "if" ->
          advance () ;
          expect TLParen ;
          let c = parse_expr () in
          expect TRParen ;
          let t = parse_block () in
          eat_kw "else" ;
          let e = parse_block () in
          If (c, t, e)
      | TIdent "while" ->
          advance () ;
          expect TLParen ;
          let c = parse_expr () in
          expect TRParen ;
          let body = parse_block () in
          While (c, body)
      | TStar ->
          (* a store: [*ptr = e;] *)
          advance () ;
          let ptr = parse_unary () in
          expect TEq ;
          let e = parse_expr () in
          expect TSemi ;
          Store (ptr, e)
      | TIdent name ->
          (* an assignment: [name = e;] *)
          advance () ;
          expect TEq ;
          let e = parse_expr () in
          expect TSemi ;
          Assign (name, e)
      | _ ->
          fail "expected a statement"
    in
    {line; desc}
  in
  let parse_params () =
    let rec loop acc =
      match peek () with
      | TRParen ->
          List.rev acc
      | _ -> (
          let x = parse_ident () in
          match peek () with
          | TComma ->
              advance () ;
              loop (x :: acc)
          | _ ->
              List.rev (x :: acc) )
    in
    match peek () with TRParen -> [] | _ -> loop []
  in
  let parse_func () =
    let line = peek_line () in
    eat_kw "fun" ;
    let name = parse_ident () in
    expect TLParen ;
    let params = parse_params () in
    expect TRParen ;
    let body = parse_block () in
    {name; params; body; line}
  in
  let rec parse_program acc =
    match peek () with TEof -> List.rev acc | _ -> parse_program (parse_func () :: acc)
  in
  parse_program []


(* ============================================================================================== *)
(*  STEP (2): TRANSLATING ToyLang -> Textual                                                      *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  This is the heart of any frontend. We turn the ToyLang AST into a [Textual.Module.t].         *)
(*                                                                                                *)
(*  MEMORY MODEL.                                                                                 *)
(*  Every ToyLang variable -- a parameter or a [var] local -- lives in its own memory             *)
(*  slot, addressed in Textual by [Lvar "x"] (read "&x"). To *read* a variable we emit a          *)
(*  load ([Exp.Load {exp = Lvar x}]); to *write* it we emit a store ([Instr.Store]). This         *)
(*  is exactly how Textual handles parameters too -- e.g. in a generated unit you see             *)
(*  [n2:int = load &untainted]. Keeping all values in memory has a wonderful consequence:         *)
(*  the only SSA temporaries (the [n0], [n1], ... idents) we ever need are short-lived and        *)
(*  never cross from one basic block to another, so we never have to manage SSA block             *)
(*  parameters ourselves.                                                                         *)
(*                                                                                                *)
(*  NESTED EFFECTS ARE OK.                                                                        *)
(*  In SIL proper, arithmetic operands and call arguments must be pure and calls must be          *)
(*  let-bound. Textual is more relaxed: we may write [*p = max(3,7)] with the call nested         *)
(*  right inside the store, or [a < b] (two loads, nested in the [if] condition). The             *)
(*  [remove_effects_in_subexprs] transform run by [TextualTransform.run_exn] hoists those         *)
(*  sub-expressions into fresh let-bound temporaries for us. So [compile_exp] below can           *)
(*  return arbitrarily nested *pure-looking* expressions and not worry about it.                  *)
(*                                                                                                *)
(*  TYPES.                                                                                        *)
(*  Textual is *typed*, and the recommended path is to make your language fit that type           *)
(*  system and run the *strict* verifier ([TextualVerification.verify_strict], as                 *)
(*  integration/Python.ml does) -- it does real type checking and inference and catches           *)
(*  mistranslations early. (The lenient [verify_keep_going] is only a fallback for when your      *)
(*  language genuinely cannot be fit into the type system.) ToyLang is untyped, so we give it     *)
(*  a tiny type discipline: integers are [int]; heap pointers are [*Cell] where                   *)
(*  [Cell = {contents: int}]. [compile_exp] therefore returns the Textual *type* of each          *)
(*  expression alongside it, and we infer each local's type from its initialiser, so every        *)
(*  slot is declared and every load/store is annotated consistently.                              *)

(* ToyLang operators map onto SIL binary operators. Textual exposes each SIL operator as a        *)
(* builtin procedure name via [ProcDecl.of_binop], and an application of it via                   *)
(* [Exp.call_non_virtual]. (Integer division is [DivI]; the [None] on +,-,* is the                *)
(* optional integer-kind, which we leave unspecified.)                                            *)
let sil_binop : binop -> Binop.t = function
  | Add ->
      Binop.PlusA None
  | Sub ->
      Binop.MinusA None
  | Mul ->
      Binop.Mult None
  | Div ->
      Binop.DivI
  | Lt ->
      Binop.Lt


(* Helpers to build Textual names. A ToyLang function [f] becomes a top-level Textual             *)
(* procedure (no enclosing class).                                                                *)
let toplevel_proc name =
  Textual.QualifiedProcName.make_qualified_proc_name Textual.QualifiedProcName.TopLevel
    (Textual.ProcName.of_string name)


(* Heap cells allocated by [alloc()] have this struct type, holding a single integer in field     *)
(* [contents]. Typing the heap precisely (instead of pretending pointers are ints) is exactly     *)
(* what lets us pass the *strict* verifier: [alloc()] yields a [*Cell], and [*p] reads/writes     *)
(* [p.contents].                                                                                  *)
let cell_type_name = Textual.TypeName.of_string "Cell"

let cell_field : Textual.qualified_fieldname =
  {enclosing_class= cell_type_name; name= Textual.FieldName.of_string "contents"}


(* The Textual type of a heap pointer: [*Cell].                                                   *)
let cell_ptr_typ = Textual.Typ.mk_ptr (Textual.Typ.Struct cell_type_name)

let loc_of line : Textual.Location.t = Known {line; col= 0}

(* The address slot of a ToyLang variable, i.e. "&x".                                             *)
let var_addr x = Textual.Exp.Lvar (Textual.VarName.of_string x)

(* Translate a ToyLang expression into a (nested) Textual expression *and* its Textual type.      *)
(* The type is needed because Textual is typed: to satisfy the strict verifier every slot must    *)
(* be declared and every load/store annotated consistently. [tenv] maps each in-scope variable    *)
(* to the type we inferred for it (see [compile_func]).                                           *)
let rec compile_exp tenv (e : exp) : Textual.Exp.t * Textual.Typ.t =
  let open Textual in
  match e with
  | Int v ->
      (Exp.Const (Const.Int (Z.of_int v)), Typ.Int)
  | Nil ->
      (* the null pointer is given a pointer type so it type-checks where a [*Cell] is wanted *)
      (Exp.Const Const.Null, cell_ptr_typ)
  | Var x ->
      let t = Map.find tenv x |> Option.value ~default:Typ.Int in
      (Exp.Load {exp= var_addr x; typ= Some t}, t)
  | Alloc ->
      (Exp.allocate_object cell_type_name, cell_ptr_typ)
  | Deref p ->
      (* [*p]: read the [contents] field of the [Cell] that [p] points at *)
      let pe, _ = compile_exp tenv p in
      (Exp.Load {exp= Exp.Field {exp= pe; field= cell_field}; typ= Some Typ.Int}, Typ.Int)
  | Binop (op, e1, e2) ->
      let c1, _ = compile_exp tenv e1 in
      let c2, _ = compile_exp tenv e2 in
      (Exp.call_non_virtual (ProcDecl.of_binop (sil_binop op)) [c1; c2], Typ.Int)
  | Call (f, args) ->
      let cargs = List.map args ~f:(fun a -> fst (compile_exp tenv a)) in
      (Exp.call_non_virtual (toplevel_proc f) cargs, Typ.Int)


(* A store instruction [addr <- value : typ].                                                     *)
let store_instr ~addr ~typ ~value ~line : Textual.Instr.t =
  Store {exp1= addr; typ= Some typ; exp2= value; loc= loc_of line}


(* A store through a ToyLang pointer [*ptr <- value]: write the [contents] field of the cell.     *)
let store_field_instr ~ptr ~value ~line : Textual.Instr.t =
  Store
    { exp1= Textual.Exp.Field {exp= ptr; field= cell_field}
    ; typ= Some Textual.Typ.Int
    ; exp2= value
    ; loc= loc_of line }


(* While translating a function we accumulate the basic blocks ("nodes") we close, the next       *)
(* fresh label number, the declared locals (with their types), and [tenv]: the inferred Textual   *)
(* type of every variable currently in scope (seeded with the parameters).                        *)
type builder =
  { mutable rev_nodes: Textual.Node.t list
  ; mutable label_ctr: int
  ; mutable locals: (Textual.VarName.t * Textual.Typ.annotated) list
  ; mutable tenv: Textual.Typ.t String.Map.t }

let fresh_label b =
  let n = b.label_ctr in
  b.label_ctr <- n + 1 ;
  Textual.NodeName.of_string (Printf.sprintf "b%d" n)


(* Record a [var x] of inferred type [typ]: remember its type for later reads, and -- unless it   *)
(* shadows a parameter or an earlier declaration -- add it to the procedure's locals.             *)
let register_local b x typ =
  if not (Map.mem b.tenv x) then
    b.locals <- (Textual.VarName.of_string x, Textual.Typ.mk_without_attributes typ) :: b.locals ;
  b.tenv <- Map.set b.tenv ~key:x ~data:typ


let mk_node ~label ~instrs ~last ~loc : Textual.Node.t =
  {label; ssa_parameters= []; exn_succs= []; last; instrs; last_loc= loc; label_loc= loc}


(* An unconditional jump terminator to a single successor label (no SSA arguments, because        *)
(* all our values live in memory).                                                                *)
let jump l : Textual.Terminator.t = Jump [{label= l; ssa_args= []}]

(* Compile a list of statements into basic blocks. Execution enters the block at label            *)
(* [entry]; when control falls off the end of [stmts] it follows the terminator [after].          *)
(* Closed nodes are pushed into [b.rev_nodes]. This is a tiny basic-block builder: we keep        *)
(* accumulating straight-line instructions into [cur_instrs] until a control-flow construct       *)
(* forces us to close the current node with a terminator and open fresh ones.                     *)
let rec compile_block b stmts ~entry ~after =
  let cur_label = ref entry in
  let cur_instrs =
    ref []
    (* reversed *)
  in
  let close ?(line = 0) last =
    b.rev_nodes <-
      mk_node ~label:!cur_label ~instrs:(List.rev !cur_instrs) ~last ~loc:(loc_of line)
      :: b.rev_nodes
  in
  let emit instr = cur_instrs := instr :: !cur_instrs in
  let compile_cond c = fst (compile_exp b.tenv c) in
  let rec go = function
    | [] ->
        (* fell off the end of the block: take the caller-provided continuation *)
        close after
    | {line; desc} :: rest -> (
      match desc with
      | Declare (x, e) ->
          let ve, vt = compile_exp b.tenv e in
          register_local b x vt ;
          emit (store_instr ~addr:(var_addr x) ~typ:vt ~value:ve ~line) ;
          go rest
      | Assign (x, e) ->
          let ve, _ = compile_exp b.tenv e in
          let typ = Map.find b.tenv x |> Option.value ~default:Textual.Typ.Int in
          emit (store_instr ~addr:(var_addr x) ~typ ~value:ve ~line) ;
          go rest
      | Store (ptr, e) ->
          let pe, _ = compile_exp b.tenv ptr in
          let ve, _ = compile_exp b.tenv e in
          emit (store_field_instr ~ptr:pe ~value:ve ~line) ;
          go rest
      | Return e ->
          (* a return ends the current block; any [rest] is unreachable and dropped *)
          let re, _ = compile_exp b.tenv e in
          close ~line (Ret re)
      | If (c, then_blk, else_blk) ->
          let then_l = fresh_label b in
          let else_l = fresh_label b in
          let join_l = fresh_label b in
          (* Close the current block with a structured [if] terminator. The                   *)
          (* [remove_if_terminator] transform later lowers this into pruned successor nodes.  *)
          close ~line (If {bexp= Exp (compile_cond c); then_= jump then_l; else_= jump else_l}) ;
          compile_block b then_blk ~entry:then_l ~after:(jump join_l) ;
          compile_block b else_blk ~entry:else_l ~after:(jump join_l) ;
          (* continue the enclosing block at the join point *)
          cur_label := join_l ;
          cur_instrs := [] ;
          go rest
      | While (c, body) ->
          let head_l = fresh_label b in
          let body_l = fresh_label b in
          let exit_l = fresh_label b in
          close ~line (jump head_l) ;
          (* the loop header tests the condition on every iteration *)
          b.rev_nodes <-
            mk_node ~label:head_l ~instrs:[]
              ~last:(If {bexp= Exp (compile_cond c); then_= jump body_l; else_= jump exit_l})
              ~loc:(loc_of line)
            :: b.rev_nodes ;
          compile_block b body ~entry:body_l ~after:(jump head_l) ;
          cur_label := exit_l ;
          cur_instrs := [] ;
          go rest )
  in
  go stmts


(* Compile a whole ToyLang function into a Textual procedure descriptor. Parameters are typed     *)
(* [int] (a real frontend would take their types from the source); each local's type is then      *)
(* inferred from its initialiser as the body is translated.                                       *)
let compile_func (f : func) : Textual.ProcDesc.t =
  let param_tenv =
    List.fold f.params ~init:String.Map.empty ~f:(fun m p -> Map.set m ~key:p ~data:Textual.Typ.Int)
  in
  let b = {rev_nodes= []; label_ctr= 0; locals= []; tenv= param_tenv} in
  let entry = fresh_label b in
  (* If a function falls off its body without an explicit [return], we return null. *)
  compile_block b f.body ~entry ~after:(Ret (Textual.Exp.Const Textual.Const.Null)) ;
  let params = List.map f.params ~f:Textual.VarName.of_string in
  let int_annot = Textual.Typ.mk_without_attributes Textual.Typ.Int in
  let procdecl : Textual.ProcDecl.t =
    { qualified_name= toplevel_proc f.name
    ; formals_types= Some (List.map f.params ~f:(fun _ -> int_annot))
    ; result_type= int_annot
    ; attributes= [] }
  in
  { procdecl
  ; nodes= List.rev b.rev_nodes
  ; fresh_ident= None (* let the transforms compute a safe fresh ident for us *)
  ; start= entry
  ; params
  ; locals= List.rev b.locals
  ; exit_loc= loc_of f.line }


(* Assemble the whole module: the source-language attribute, the [Cell] heap-cell struct,         *)
(* and one procedure per ToyLang function. We tag the module as C -- ToyLang has no dedicated     *)
(* [Lang.t], and C is the closest imperative match.                                               *)
let build_module sourcefile (prog : program) : Textual.Module.t =
  let cell =
    Textual.Module.Struct
      { name= cell_type_name
      ; supers= []
      ; fields= [{qualified_name= cell_field; typ= Textual.Typ.Int; attributes= []}]
      ; attributes= [] }
  in
  let procs = List.map prog ~f:(fun f -> Textual.Module.Proc (compile_func f)) in
  {attrs= [Textual.Attr.mk_source_language Textual.Lang.C]; decls= cell :: procs; sourcefile}


(* >>> RUN IT: unit/TutoFrontendTest.ml, test "translation to Textual".                           *)
(*     That is steps (1)+(2) end-to-end -- it parses the example program and prints the           *)
(*     [Textual.Module.t] this code produces (nested [load &x], [__sil_lt(...)] calls, the        *)
(*     structured [if] terminator, the [Cell] struct). Read that output, then come back.          *)

(* ============================================================================================== *)
(*  STEPS (3)-(6): THE REUSABLE BACKEND PIPELINE                                                  *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  From here on nothing is ToyLang-specific: this is the exact same dance every Textual          *)
(*  frontend does (compare with infer/src/integration/Python.ml). We verify, run the              *)
(*  standard transforms, lower to SIL, and capture.                                               *)

let capture_string ~source_path ~content =
  let prog = parse content in
  let sourcefile = Textual.SourceFile.create source_path in
  let module_ = build_module sourcefile prog in
  (* (3) strict verification: full type checking + inference. This is the recommended path        *)
  (*     (same as integration/Python.ml). If ToyLang could not be fit into Textual's type system  *)
  (*     we would fall back to [TextualVerification.verify_keep_going ~lenient:true].             *)
  (* >>> RUN IT: unit/TutoFrontendTest.ml, test "strict verification succeeds" -- confirms our    *)
  (*     typed ToyLang really fits Textual's type system (no type errors).                        *)
  match TextualVerification.verify_strict module_ with
  | Error errors ->
      List.iter errors ~f:(fun e ->
          L.external_error "ToyLang: %a@\n"
            (TextualVerification.pp_error_with_sourcefile sourcefile)
            e ) ;
      L.die ExternalError "ToyLang: Textual verification failed for %s" source_path
  | Ok verified -> (
      (* (4) the standard transform pipeline: remove_if_terminator, remove_effects_in_subexprs,   *)
      (*     let_propagation, out_of_ssa -- this is what makes our nested expressions legal SIL.  *)
      (* >>> RUN IT: unit/TutoFrontendTest.ml, test "after verification and transforms" --        *)
      (*     it prints the module after this exact line, so you can diff it against the           *)
      (*     pre-transform output and see the effects get hoisted and the [if] get pruned.        *)
      let transformed, decls = TextualTransform.run_exn Textual.Lang.C verified in
      (* (5) lower Textual to Infer's SIL: a control-flow graph plus a type environment.          *)
      (* >>> RUN IT: unit/TutoFrontendTest.ml, test "lowering to SIL succeeds" -- the proof       *)
      (*     this whole frontend is well-formed (SIL conversion succeeds for max and main).       *)
      match TextualSil.module_to_sil Textual.Lang.C transformed decls with
      | Error errors ->
          List.iter errors ~f:(fun e ->
              L.external_error "%a@\n" (Textual.pp_transform_error sourcefile) e ) ;
          L.die ExternalError "ToyLang: Textual-to-SIL conversion failed for %s" source_path
      | Ok (cfg, tenv) ->
          (* (6) store the captured CFG and type environment so the analysers can run. *)
          let sil = {TextualParser.TextualFile.sourcefile; cfg; tenv} in
          TextualParser.TextualFile.capture ~textual_module:transformed ~use_global_tenv:true sil ;
          Tenv.Global.store ~normalize:true tenv )


(* Read a [.toy] file from disk and capture it.                                                   *)
let capture_file path = capture_string ~source_path:path ~content:(In_channel.read_all path)

(* ============================================================================================== *)
(*  STEP (7): WIRING IT INTO THE infer DRIVER                                                     *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  The code above compiles as part of [Textuallib], but on its own nothing calls                 *)
(*  [capture_file]. To expose a real `infer --capture-toylang foo.toy` subcommand we added        *)
(*  four small pieces, mirroring exactly how the existing frontends are wired. These are real     *)
(*  files in the tree -- go read them:                                                            *)
(*                                                                                                *)
(*  a) THE DRIVER ENTRY POINT -- infer/src/integration/ToyLang.ml (+ .mli).                       *)
(*     A tiny module exposing [val capture : files:string list -> unit] that just maps            *)
(*     [TutoFrontend.capture_file] over the input files. This is the conventional home for a      *)
(*     frontend's driver glue (next to Hack.ml, Python.ml). It lives in the                       *)
(*     [Integration] library, which depends on [Textuallib], so it can call [TutoFrontend].       *)
(*                                                                                                *)
(*  b) THE CLI OPTION -- infer/src/base/Config.ml (+ .mli).                                       *)
(*     [and capture_toylang = CLOpt.mk_path_list ~long:"capture-toylang" ...] declares the        *)
(*     [--capture-toylang] flag; a line [and capture_toylang = RevList.to_list !capture_toylang]  *)
(*     near the bottom exposes its value; and [Config.mli] gets [val capture_toylang : ...].      *)
(*                                                                                                *)
(*  c) THE DRIVER DISPATCH -- infer/src/integration/Driver.ml (+ .mli).                           *)
(*     A new capture [mode] constructor [ToyLang of {files: string list}]; a [pp_mode] case;      *)
(*     a selection rule in [mode_of_build_command]                                                *)
(*         | [] when not (List.is_empty Config.capture_toylang) ->                                *)
(*             ToyLang {files= Config.capture_toylang}                                            *)
(*     and the dispatch in [capture]: [| ToyLang {files} -> ToyLang.capture ~files].              *)
(*                                                                                                *)
(*  d) dune: no change needed -- neither [infer/src/textual/dune] nor the [Integration] dune      *)
(*     lists modules explicitly, so [TutoFrontend.ml] and [ToyLang.ml] are picked up              *)
(*     automatically.                                                                             *)
(*                                                                                                *)
(*  --------------------------------------------------------------------------------------------- *)
(*  END-TO-END TEST -- infer/tests/codetoanalyze/textual/tuto/                                    *)
(*  --------------------------------------------------------------------------------------------- *)
(*  A standard codetoanalyze test exercises the *whole* chain (capture + Pulse):                  *)
(*    - [bug.toy]      : ToyLang source with [addOk] (clean), [allocNoFreeBad] (leak) and         *)
(*                       [derefNilBad] (null deref)                                               *)
(*    - [Makefile]    : runs [infer --capture-toylang *.toy --pulse-only ...], then diffs         *)
(*                      the report against [issues.exp]                                           *)
(*    - [issues.exp]  : the expected report -- MEMORY_LEAK_CPP in [allocNoFreeBad] and            *)
(*                      NULLPTR_DEREFERENCE in [derefNilBad].                                     *)
(*  It is registered in the root Makefile as [DIRECT_TESTS += textual_tuto]. Run it with:         *)
(*                                                                                                *)
(*      make direct_textual_tuto_test      # build infer + run the test                           *)
(*      make direct_textual_tuto_replace   # update issues.exp after intentional changes          *)
(*                                                                                                *)
(*  --------------------------------------------------------------------------------------------- *)
(*  TRYING IT BY HAND                                                                             *)
(*  --------------------------------------------------------------------------------------------- *)
(*      infer --capture-toylang bug.toy && infer analyze                                          *)
(*                                                                                                *)
(*  Pulse reports the null dereference in [derefNilBad]. Useful debugging knobs:                  *)
(*      --dump-textual   dump the generated .sil so you can read the Textual we produced          *)
(*      --debug          full per-procedure HTML traces under infer-out/captured/                 *)
(* ============================================================================================== *)

(* ============================================================================================== *)
(*  APPENDIX: THE OTHER WAY IN -- LOADING TEXTUAL FROM *TEXT*                                     *)
(* ============================================================================================== *)
(*                                                                                                *)
(*  This whole tutorial builds the [Textual.Module.t] *in process*, directly in OCaml.            *)
(*  But that is only one of two ways to feed Textual to Infer. Textual is also a concrete         *)
(*  *textual* language (the [.sil] syntax, parsed by TextualMenhir.mly / TextualLexer.ml),        *)
(*  so a frontend can instead be an *external tool* that emits Textual as text, which Infer       *)
(*  then parses. This is attractive when your translator is most naturally written outside        *)
(*  OCaml.                                                                                        *)
(*                                                                                                *)
(*  IN-PROCESS (this tutorial, LLVM , and Python)    OUT-OF-PROCESS (Hack)                        *)
(*    your OCaml code builds Textual.Module.t         external compiler prints Textual text       *)
(*               |                                              |                                 *)
(*               |                                    TextualParser.parse_string / .parse         *)
(*               |                                    turns the TEXT into Textual.Module.t        *)
(*               +----------------------+-----------------------+                                 *)
(*                                      v                                                         *)
(*                 (3) verify -> (4) transform -> (5) module_to_sil -> (6) capture                *)
(*                              (identical backend from here on)                                  *)
(*                                                                                                *)
(*  THE HACK PIPELINE -- read infer/src/integration/Hack.ml.                                      *)
(*  Hack's frontend is a separate compiler, [hackc] (written in Rust), invoked by Infer as a      *)
(*  subprocess with the [compile-infer] subcommand ([start_hackc] / [compile]). [hackc] writes    *)
(*  Textual *source* to its stdout -- one [.sil] "unit" per Hack file, framed by marker lines     *)
(*  [// TEXTUAL UNIT START/END/COUNT] ([module OutputLine], [module Unit]). For each unit Infer:  *)
(*    a) wraps the text as [TextualParser.TextualFile.TranslatedFile {source_path; content;       *)
(*       line_map}]. The [line_map] ([LineMap.create content]) maps generated-Textual line        *)
(*       numbers back to the original Hack source lines, so Pulse reports point at the real       *)
(*       source rather than at the generated [.sil].                                              *)
(*    b) [TextualFile.parse] runs the Textual *parser* to obtain a [Textual.Module.t] -- this is  *)
(*       the step the in-process path skips, since we built the module ourselves.                 *)
(*    c) then the very same backend: [fix_hackc_mistranslations] (a Hack-specific cleanup),       *)
(*       [TextualFile.verify], [TextualFile.textual_to_sil], [TextualFile.capture].               *)
(*                                                                                                *)
(*  THE SIMPLEST TEXT ENTRY POINT -- [--capture-textual foo.sil].                                 *)
(*  You do not even need a custom integration to load a hand-written or tool-generated [.sil]:    *)
(*  the built-in [Textual] driver mode wraps each file as [TextualFile.StandaloneFile] and feeds  *)
(*  it to [TextualParser.textual_frontend_capture]. This is the quickest way to experiment: write *)
(*  Textual by hand, save it as [foo.sil] with a [.source_language = "..."] header, and run       *)
(*  [infer --capture-textual foo.sil && infer analyze]. (Hack also uses [StandaloneFile] this way *)
(*  to load its hand-written Textual models, see [load_textual_models].)                          *)
(*                                                                                                *)
(*  WHICH TO CHOOSE? Build in process (like here) when your translator lives in OCaml and you     *)
(*  want type-safe construction and no parsing round-trip. Emit text (like Hack) when the natural *)
(*  home for your translator is another language/toolchain; you pay for a parse, but you decouple *)
(*  the frontend from the OCaml build and get a human-readable artifact for free.                 *)
(* ============================================================================================== *)
