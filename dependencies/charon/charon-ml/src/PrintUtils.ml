open Types
open Expressions
open GAst

let option_to_string (to_string : 'a -> string) (x : 'a option) : string =
  match x with
  | Some x -> "Some (" ^ to_string x ^ ")"
  | None -> "None"

let block_id_to_string (id : UllbcAst.BlockId.id) : string =
  "block@" ^ UllbcAst.BlockId.to_string id

(** The formatting environment can be incomplete: if some information is missing
    (for instance we can't find the type variable for a given index) we print
    the id in raw format. *)
type 'fun_body fmt_env = {
  crate : 'fun_body gcrate;
  generics : generic_params list;
      (** We have a stack of generic parameters, because we can dive into
          binders (for instance because of the arrow type). *)
  locals : (local_id * string option) list;
      (** The local variables don't need to be ordered (same as the generics) *)
}

let of_crate (crate : 'fun_body gcrate) : 'fun_body fmt_env =
  { crate; generics = []; locals = [] }

let fmt_env_update_generics_and_preds (env : 'a fmt_env)
    (generics : generic_params) : 'a fmt_env =
  { env with generics = generics :: env.generics }

let fmt_env_push_regions (env : 'a fmt_env) (regions : region_var list) :
    'a fmt_env =
  {
    env with
    generics = { TypesUtils.empty_generic_params with regions } :: env.generics;
  }
