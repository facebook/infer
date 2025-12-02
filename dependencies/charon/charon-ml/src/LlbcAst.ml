open Types
open Values
open Expressions
open Meta
include GAst
include Generated_LlbcAst

type expr_body = block gexpr_body [@@deriving show]
type fun_body = expr_body [@@deriving show]
type fun_decl = block gfun_decl [@@deriving show]

(** LLBC crate *)
type crate = block gcrate [@@deriving show]

(* Ancestors for the type_decl visitors *)
class ['self] iter_statement =
  object (self : 'self)
    inherit [_] iter_statement_base

    method! visit_block : 'env -> block -> unit =
      fun env block -> self#visit_block_suffix env block.statements

    (* Visit all the suffixes of the block, i.e. successive tails of the list of statements. *)
    method visit_block_suffix : 'env -> statement list -> unit =
      fun env stmts ->
        match stmts with
        | [] -> ()
        | hd :: tl ->
            self#visit_statement env hd;
            self#visit_block_suffix env tl
  end

(* Ancestors for the type_decl visitors *)
class ['self] map_statement =
  object (self : 'self)
    inherit [_] map_statement_base

    method! visit_block : 'env -> block -> block =
      fun env block ->
        let statements = self#visit_block_suffix env block.statements in
        { span = block.span; statements }

    (* Visit all the suffixes of the block, i.e. successive tails of the list of statements. *)
    method visit_block_suffix : 'env -> statement list -> statement list =
      fun env stmts ->
        match stmts with
        | [] -> []
        | hd :: tl ->
            self#visit_statement env hd :: self#visit_block_suffix env tl
  end
