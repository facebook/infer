(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language *)

open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

(** {2 Programs and Types} *)

(** Kind of prune instruction *)
type if_kind =
  | Ik_bexp  (** boolean expressions, and exp ? exp : exp *)
  | Ik_dowhile
  | Ik_for
  | Ik_if
  | Ik_land_lor  (** obtained from translation of && or || *)
  | Ik_while
  | Ik_switch
[@@deriving compare]

(** An instruction. *)
type instr =
  (* Note for frontend writers:
     [x] must be used in a subsequent instruction, otherwise the entire
     `Load` instruction may be eliminated by copy-propagation. *)
  | Load of Ident.t * Exp.t * Typ.t * Location.t
      (** Load a value from the heap into an identifier.
          [x = *lexp:typ] where
            [lexp] is an expression denoting a heap address
            [typ] is the root type of [lexp]. *)
  | Store of Exp.t * Typ.t * Exp.t * Location.t
      (** Store the value of an expression into the heap.
          [*lexp1:typ = exp2] where
            [lexp1] is an expression denoting a heap address
            [typ] is the root type of [lexp1]
            [exp2] is the expression whose value is store. *)
  | Prune of Exp.t * Location.t * bool * if_kind
      (** prune the state based on [exp=1], the boolean indicates whether true branch *)
  | Call of (Ident.t * Typ.t) * Exp.t * (Exp.t * Typ.t) list * Location.t * CallFlags.t
      (** [Call ((ret_id, ret_typ), e_fun, arg_ts, loc, call_flags)] represents an instruction
          [ret_id = e_fun(arg_ts);] *)
  | Nullify of Pvar.t * Location.t  (** nullify stack variable *)
  | Abstract of Location.t  (** apply abstraction *)
  | ExitScope of Var.t list * Location.t  (** remove temporaries and dead program variables *)
[@@deriving compare]

let equal_instr = [%compare.equal: instr]

let skip_instr = ExitScope ([], Location.dummy)

(** Check if an instruction is auxiliary, or if it comes from source instructions. *)
let instr_is_auxiliary = function
  | Load _ | Store _ | Prune _ | Call _ ->
      false
  | Nullify _ | Abstract _ | ExitScope _ ->
      true


(** offset for an lvalue *)
type offset = Off_fld of Typ.Fieldname.t * Typ.t | Off_index of Exp.t

(** {2 Components of Propositions} *)

(** an atom is a pure atomic formula *)
type atom =
  | Aeq of Exp.t * Exp.t  (** equality *)
  | Aneq of Exp.t * Exp.t  (** disequality *)
  | Apred of PredSymb.t * Exp.t list  (** predicate symbol applied to exps *)
  | Anpred of PredSymb.t * Exp.t list  (** negated predicate symbol applied to exps *)
[@@deriving compare]

let equal_atom = [%compare.equal: atom]

let atom_has_local_addr a =
  match a with
  | Aeq (e0, e1) | Aneq (e0, e1) ->
      Exp.has_local_addr e0 || Exp.has_local_addr e1
  | Apred _ | Anpred _ ->
      false


(** kind of lseg or dllseg predicates *)
type lseg_kind =
  | Lseg_NE  (** nonempty (possibly circular) listseg *)
  | Lseg_PE  (** possibly empty (possibly circular) listseg *)
[@@deriving compare]

let equal_lseg_kind = [%compare.equal: lseg_kind]

(** The boolean is true when the pointer was dereferenced without testing for zero. *)
type zero_flag = bool option [@@deriving compare]

(** True when the value was obtained by doing case analysis on null in a procedure call. *)
type null_case_flag = bool [@@deriving compare]

(** instrumentation of heap values *)
type inst =
  | Iabstraction
  | Iactual_precondition
  | Ialloc
  | Iformal of zero_flag * null_case_flag
  | Iinitial
  | Ilookup
  | Inone
  | Inullify
  | Irearrange of zero_flag * null_case_flag * int * PredSymb.path_pos
  | Itaint
  | Iupdate of zero_flag * null_case_flag * int * PredSymb.path_pos
  | Ireturn_from_call of int
[@@deriving compare]

let equal_inst = [%compare.equal: inst]

(** structured expressions represent a value of structured type, such as an array or a struct. *)
type 'inst strexp0 =
  | Eexp of Exp.t * 'inst  (** Base case: expression with instrumentation *)
  | Estruct of (Typ.Fieldname.t * 'inst strexp0) list * 'inst  (** C structure *)
  | Earray of Exp.t * (Exp.t * 'inst strexp0) list * 'inst
      (** Array of given length
          There are two conditions imposed / used in the array case.
          First, if some index and value pair appears inside an array
          in a strexp, then the index is less than the length of the array.
          For instance, x |->[10 | e1: v1] implies that e1 <= 9.
          Second, if two indices appear in an array, they should be different.
          For instance, x |->[10 | e1: v1, e2: v2] implies that e1 != e2. *)
[@@deriving compare]

type strexp = inst strexp0

let compare_strexp ?(inst = false) se1 se2 =
  compare_strexp0 (match inst with true -> compare_inst | false -> fun _ _ -> 0) se1 se2


let equal_strexp ?(inst = false) se1 se2 = Int.equal (compare_strexp ~inst se1 se2) 0

(** an atomic heap predicate *)
type 'inst hpred0 =
  | Hpointsto of Exp.t * 'inst strexp0 * Exp.t
      (** represents [exp|->strexp:typexp] where [typexp]
      is an expression representing a type, e.h. [sizeof(t)]. *)
  | Hlseg of lseg_kind * 'inst hpara0 * Exp.t * Exp.t * Exp.t list
      (** higher - order predicate for singly - linked lists.
      Should ensure that exp1!= exp2 implies that exp1 is allocated.
      This assumption is used in the rearrangement. The last [exp list] parameter
      is used to denote the shared links by all the nodes in the list. *)
  | Hdllseg of lseg_kind * 'inst hpara_dll0 * Exp.t * Exp.t * Exp.t * Exp.t * Exp.t list
      (** higher-order predicate for doubly-linked lists.
      Parameter for the higher-order singly-linked list predicate.
      Means "lambda (root,next,svars). Exists evars. body".
      Assume that root, next, svars, evars are disjoint sets of
      primed identifiers, and include all the free primed identifiers in body.
      body should not contain any non - primed identifiers or program
      variables (i.e. pvars). *)
[@@deriving compare]

and 'inst hpara0 =
  {root: Ident.t; next: Ident.t; svars: Ident.t list; evars: Ident.t list; body: 'inst hpred0 list}
[@@deriving compare]

(** parameter for the higher-order doubly-linked list predicates.
    Assume that all the free identifiers in body_dll should belong to
    cell, blink, flink, svars_dll, evars_dll. *)
and 'inst hpara_dll0 =
  { cell: Ident.t  (** address cell *)
  ; blink: Ident.t  (** backward link *)
  ; flink: Ident.t  (** forward link *)
  ; svars_dll: Ident.t list
  ; evars_dll: Ident.t list
  ; body_dll: 'inst hpred0 list }
[@@deriving compare]

type hpred = inst hpred0

(** Comparison between heap predicates. Reverse natural order, and order first by anchor exp. *)
let compare_hpred ?(inst = false) hpred1 hpred2 =
  compare_hpred0 (match inst with true -> compare_inst | false -> fun _ _ -> 0) hpred1 hpred2


let equal_hpred ?(inst = false) hpred1 hpred2 = Int.equal (compare_hpred ~inst hpred1 hpred2) 0

type hpara = inst hpara0

let compare_hpara = compare_hpara0 (fun _ _ -> 0)

let equal_hpara = [%compare.equal: hpara]

type hpara_dll = inst hpara_dll0

let compare_hpara_dll = compare_hpara_dll0 (fun _ _ -> 0)

let equal_hpara_dll = [%compare.equal: hpara_dll]

(** {2 Comparision and Inspection Functions} *)
let is_objc_object = function
  | Hpointsto (_, _, Sizeof {typ}) ->
      Typ.is_objc_class typ
  | _ ->
      false


(** Check if a pvar is a local static in objc *)
let is_static_local_name pname pvar =
  (* local static name is of the form procname_varname *)
  let var_name = Mangled.to_string (Pvar.get_name pvar) in
  match Str.split_delim (Str.regexp_string pname) var_name with [_; _] -> true | _ -> false


(** {2 Sets of heap predicates} *)
module HpredSet = Caml.Set.Make (struct
  type t = hpred

  let compare = compare_hpred ~inst:false
end)

(** {2 Pretty Printing} *)

let color_wrapper pe ppf x ~f =
  if Config.print_using_diff && pe.Pp.kind <> Pp.TEXT then
    let color = pe.Pp.cmap_norm (Obj.repr x) in
    if color <> pe.Pp.color then
      let pe' =
        if Pp.equal_color color Pp.Red then
          (* All subexpressions red *)
          Pp.{pe with cmap_norm= colormap_red; color= Red}
        else Pp.{pe with color}
      in
      Io_infer.Html.with_color color (f pe') ppf x
    else f pe ppf x
  else f pe ppf x


(** Print a sequence with difference mode if enabled. *)
let pp_seq_diff pp pe0 f =
  if not Config.print_using_diff then Pp.comma_seq pp f
  else
    let rec doit = function
      | [] ->
          ()
      | [x] ->
          color_wrapper pe0 f x ~f:(fun _pe -> pp)
      | x :: l ->
          color_wrapper pe0 f x ~f:(fun _pe -> pp) ;
          F.pp_print_string f ", " ;
          doit l
    in
    doit


(** Pretty print an expression. *)
let pp_exp_printenv ?(print_types = false) =
  color_wrapper ~f:(fun pe f e0 ->
      let e =
        match pe.Pp.obj_sub with
        | Some sub ->
            (* apply object substitution to expression *) Obj.obj (sub (Obj.repr e0))
        | None ->
            e0
      in
      if not (Exp.equal e0 e) then
        match e with Exp.Lvar pvar -> Pvar.pp_value f pvar | _ -> assert false
      else Exp.pp_printenv ~print_types pe f e )


(** dump an expression. *)
let d_exp (e : Exp.t) = L.d_pp_with_pe pp_exp_printenv e

(** Pretty print a list of expressions. *)
let pp_exp_list pe f expl = Pp.seq (pp_exp_printenv pe) f expl

(** dump a list of expressions. *)
let d_exp_list (el : Exp.t list) = L.d_pp_with_pe pp_exp_list el

let pp_texp pe f = function
  | Exp.Sizeof {typ; nbytes; dynamic_length; subtype} ->
      let pp_len f l = Option.iter ~f:(F.fprintf f "[%a]" (pp_exp_printenv pe)) l in
      let pp_size f size = Option.iter ~f:(Int.pp f) size in
      F.fprintf f "%a%a%a%a" (Typ.pp pe) typ pp_size nbytes pp_len dynamic_length Subtype.pp
        subtype
  | e ->
      pp_exp_printenv pe f e


(** Pretty print a type with all the details. *)
let pp_texp_full pe f = function
  | Exp.Sizeof {typ; nbytes; dynamic_length; subtype} ->
      let pp_len f l = Option.iter ~f:(F.fprintf f "[%a]" (pp_exp_printenv pe)) l in
      let pp_size f size = Option.iter ~f:(Int.pp f) size in
      F.fprintf f "%a%a%a%a" (Typ.pp_full pe) typ pp_size nbytes pp_len dynamic_length Subtype.pp
        subtype
  | e ->
      Exp.pp_printenv ~print_types:true pe f e


(** Dump a type expression with all the details. *)
let d_texp_full (te : Exp.t) = L.d_pp_with_pe pp_texp_full te

(** Pretty print an offset *)
let pp_offset pe f = function
  | Off_fld (fld, _) ->
      Typ.Fieldname.pp f fld
  | Off_index exp ->
      (pp_exp_printenv pe) f exp


(** Pretty print a list of offsets *)
let rec pp_offset_list pe f = function
  | [] ->
      ()
  | [off1; off2] ->
      F.fprintf f "%a.%a" (pp_offset pe) off1 (pp_offset pe) off2
  | off :: off_list ->
      F.fprintf f "%a.%a" (pp_offset pe) off (pp_offset_list pe) off_list


(** Dump a list of offsets *)
let d_offset_list (offl : offset list) = L.d_pp_with_pe pp_offset_list offl

let pp_exp_typ pe f (e, t) = F.fprintf f "%a:%a" (pp_exp_printenv pe) e (Typ.pp pe) t

(** Get the location of the instruction *)
let instr_get_loc = function
  | Load (_, _, _, loc)
  | Store (_, _, _, loc)
  | Prune (_, loc, _, _)
  | Call (_, _, _, loc, _)
  | Nullify (_, loc)
  | Abstract loc
  | ExitScope (_, loc) ->
      loc


(** get the expressions occurring in the instruction *)
let instr_get_exps = function
  | Load (id, e, _, _) ->
      [Exp.Var id; e]
  | Store (e1, _, e2, _) ->
      [e1; e2]
  | Prune (cond, _, _, _) ->
      [cond]
  | Call ((id, _), e, _, _, _) ->
      [e; Exp.Var id]
  | Nullify (pvar, _) ->
      [Exp.Lvar pvar]
  | Abstract _ ->
      []
  | ExitScope (vars, _) ->
      List.map ~f:Var.to_exp vars


(** Convert an if_kind to string  *)
let if_kind_to_string = function
  | Ik_bexp ->
      "boolean exp"
  | Ik_dowhile ->
      "do while"
  | Ik_for ->
      "for loop"
  | Ik_if ->
      "if"
  | Ik_land_lor ->
      "obtained from && or ||"
  | Ik_while ->
      "while"
  | Ik_switch ->
      "switch"


(** Pretty print an instruction. *)
let pp_instr ~print_types pe0 f instr =
  let pp_typ = if print_types then Typ.pp_full else Typ.pp in
  color_wrapper pe0 f instr ~f:(fun pe f instr ->
      match instr with
      | Load (id, e, t, loc) ->
          F.fprintf f "%a=*%a:%a [%a]" Ident.pp id (pp_exp_printenv ~print_types pe) e (pp_typ pe0)
            t Location.pp loc
      | Store (e1, t, e2, loc) ->
          F.fprintf f "*%a:%a=%a [%a]" (pp_exp_printenv ~print_types pe) e1 (pp_typ pe0) t
            (pp_exp_printenv ~print_types pe) e2 Location.pp loc
      | Prune (cond, loc, true_branch, _) ->
          F.fprintf f "PRUNE(%a, %b); [%a]" (pp_exp_printenv ~print_types pe) cond true_branch
            Location.pp loc
      | Call ((id, _), e, arg_ts, loc, cf) ->
          F.fprintf f "%a=" Ident.pp id ;
          F.fprintf f "%a(%a)%a [%a]" (pp_exp_printenv ~print_types pe) e
            (Pp.comma_seq (pp_exp_typ pe))
            arg_ts CallFlags.pp cf Location.pp loc
      | Nullify (pvar, loc) ->
          F.fprintf f "NULLIFY(%a); [%a]" (Pvar.pp pe) pvar Location.pp loc
      | Abstract loc ->
          F.fprintf f "APPLY_ABSTRACTION; [%a]" Location.pp loc
      | ExitScope (vars, loc) ->
          F.fprintf f "EXIT_SCOPE(%a); [%a]" (Pp.seq ~sep:"," Var.pp) vars Location.pp loc )


let add_with_block_parameters_flag instr =
  match instr with
  | Call (ret_id_typ, Exp.Const (Const.Cfun pname), arg_ts, loc, cf) ->
      if
        List.exists ~f:(fun (exp, _) -> Exp.is_objc_block_closure exp) arg_ts
        && Typ.Procname.is_clang pname
        (* to be extended to other methods *)
      then
        let cf' = {cf with cf_with_block_parameters= true} in
        Call (ret_id_typ, Exp.Const (Const.Cfun pname), arg_ts, loc, cf')
      else instr
  | _ ->
      instr


(** Check if a pvar is a local pointing to a block in objc *)
let is_block_pvar pvar = Typ.has_block_prefix (Mangled.to_string (Pvar.get_name pvar))

(** Dump an instruction. *)
let d_instr (i : instr) = L.d_pp_with_pe ~color:Pp.Green (pp_instr ~print_types:true) i

let pp_atom =
  color_wrapper ~f:(fun pe f a ->
      match a with
      | Aeq (BinOp (op, e1, e2), Const (Cint i)) when IntLit.isone i ->
          (pp_exp_printenv pe) f (Exp.BinOp (op, e1, e2))
      | Aeq (e1, e2) ->
          F.fprintf f "%a = %a" (pp_exp_printenv pe) e1 (pp_exp_printenv pe) e2
      | Aneq (e1, e2) ->
          F.fprintf f "%a != %a" (pp_exp_printenv pe) e1 (pp_exp_printenv pe) e2
      | Apred (a, es) ->
          F.fprintf f "%s(%a)" (PredSymb.to_string pe a) (Pp.comma_seq (pp_exp_printenv pe)) es
      | Anpred (a, es) ->
          F.fprintf f "!%s(%a)" (PredSymb.to_string pe a) (Pp.comma_seq (pp_exp_printenv pe)) es )


(** dump an atom *)
let d_atom (a : atom) = L.d_pp_with_pe pp_atom a

let pp_lseg_kind f = function Lseg_NE -> F.pp_print_string f "ne" | Lseg_PE -> ()

(** Print a *-separated sequence. *)
let pp_star_seq pp f l = Pp.seq ~sep:" * " pp f l

(** Module Predicates records the occurrences of predicates as parameters
    of (doubly -)linked lists and Epara. Provides unique numbering
    for predicates and an iterator. *)
module Predicates : sig
  (** predicate environment *)

  type env

  (** create an empty predicate environment *)

  val empty_env : unit -> env

  (** return true if the environment is empty *)

  val is_empty : env -> bool

  (** return the id of the hpara *)

  val get_hpara_id : env -> hpara -> int

  (** return the id of the hpara_dll *)

  val get_hpara_dll_id : env -> hpara_dll -> int

  (** [iter env f f_dll] iterates [f] and [f_dll] on all the hpara and hpara_dll,
      passing the unique id to the functions. The iterator can only be used once. *)

  val iter : env -> (int -> hpara -> unit) -> (int -> hpara_dll -> unit) -> unit

  (** Process one hpred, updating the predicate environment *)

  val process_hpred : env -> hpred -> unit
end = struct
  (** hash tables for hpara *)
  module HparaHash = Hashtbl.Make (struct
    type t = hpara

    let equal = equal_hpara

    let hash = Hashtbl.hash
  end)

  (** hash tables for hpara_dll *)
  module HparaDllHash = Hashtbl.Make (struct
    type t = hpara_dll

    let equal = equal_hpara_dll

    let hash = Hashtbl.hash
  end)

  (** Map each visited hpara to a unique number and a boolean denoting whether it has been emitted,
      also keep a list of hparas still to be emitted. Same for hpara_dll. *)
  type env =
    { mutable num: int
    ; hash: (int * bool) HparaHash.t
    ; mutable todo: hpara list
    ; hash_dll: (int * bool) HparaDllHash.t
    ; mutable todo_dll: hpara_dll list }

  (** return true if the environment is empty *)
  let is_empty env = Int.equal env.num 0

  (** return the id of the hpara *)
  let get_hpara_id env hpara = fst (HparaHash.find env.hash hpara)

  (** return the id of the hpara_dll *)
  let get_hpara_dll_id env hpara_dll = fst (HparaDllHash.find env.hash_dll hpara_dll)

  (** Process one hpara, updating the map from hparas to numbers, and the todo list *)
  let process_hpara env hpara =
    if not (HparaHash.mem env.hash hpara) then (
      HparaHash.add env.hash hpara (env.num, false) ;
      env.num <- env.num + 1 ;
      env.todo <- env.todo @ [hpara] )


  (** Process one hpara_dll, updating the map from hparas to numbers, and the todo list *)
  let process_hpara_dll env hpara_dll =
    if not (HparaDllHash.mem env.hash_dll hpara_dll) then (
      HparaDllHash.add env.hash_dll hpara_dll (env.num, false) ;
      env.num <- env.num + 1 ;
      env.todo_dll <- env.todo_dll @ [hpara_dll] )


  (** Process a sexp, updating env *)
  let rec process_sexp env = function
    | Eexp _ ->
        ()
    | Earray (_, esel, _) ->
        List.iter ~f:(fun (_, se) -> process_sexp env se) esel
    | Estruct (fsel, _) ->
        List.iter ~f:(fun (_, se) -> process_sexp env se) fsel


  (** Process one hpred, updating env *)
  let rec process_hpred env = function
    | Hpointsto (_, se, _) ->
        process_sexp env se
    | Hlseg (_, hpara, _, _, _) ->
        List.iter ~f:(process_hpred env) hpara.body ;
        process_hpara env hpara
    | Hdllseg (_, hpara_dll, _, _, _, _, _) ->
        List.iter ~f:(process_hpred env) hpara_dll.body_dll ;
        process_hpara_dll env hpara_dll


  (** create an empty predicate environment *)
  let empty_env () =
    {num= 0; hash= HparaHash.create 3; todo= []; hash_dll= HparaDllHash.create 3; todo_dll= []}


  (** iterator for predicates which are marked as todo in env,
      unless they have been visited already.
      This can in turn extend the todo list for the nested predicates,
      which are then visited as well.
      Can be applied only once, as it destroys the todo list *)
  let iter (env : env) f f_dll =
    while env.todo <> [] || env.todo_dll <> [] do
      match env.todo with
      | hpara :: todo' ->
          env.todo <- todo' ;
          let n, emitted = HparaHash.find env.hash hpara in
          if not emitted then f n hpara
      | [] -> (
        match env.todo_dll with
        | hpara_dll :: todo_dll' ->
            env.todo_dll <- todo_dll' ;
            let n, emitted = HparaDllHash.find env.hash_dll hpara_dll in
            if not emitted then f_dll n hpara_dll
        | [] ->
            () )
    done
end

let pp_texp_simple pe =
  match pe.Pp.opt with SIM_DEFAULT -> pp_texp pe | SIM_WITH_TYP -> pp_texp_full pe


let inst_actual_precondition = Iactual_precondition

(** for formal parameters *)
let inst_formal = Iformal (None, false)

(** for initial values *)
let inst_initial = Iinitial

let inst_lookup = Ilookup

let inst_none = Inone

let inst_nullify = Inullify

let inst_rearrange b loc pos = Irearrange (Some b, false, loc.Location.line, pos)

let inst_update loc pos = Iupdate (None, false, loc.Location.line, pos)

(** update the location of the instrumentation *)
let inst_new_loc loc inst =
  match inst with
  | Iabstraction ->
      inst
  | Iactual_precondition ->
      inst
  | Ialloc ->
      inst
  | Iformal _ ->
      inst
  | Iinitial ->
      inst
  | Ilookup ->
      inst
  | Inone ->
      inst
  | Inullify ->
      inst
  | Irearrange (zf, ncf, _, pos) ->
      Irearrange (zf, ncf, loc.Location.line, pos)
  | Itaint ->
      inst
  | Iupdate (zf, ncf, _, pos) ->
      Iupdate (zf, ncf, loc.Location.line, pos)
  | Ireturn_from_call _ ->
      Ireturn_from_call loc.Location.line


(** pretty-print an inst *)
let pp_inst f inst =
  let pp_zero_flag f = function Some true -> F.pp_print_string f "(z)" | _ -> () in
  let pp_null_case_flag f ncf = if ncf then F.pp_print_string f "(ncf)" in
  match inst with
  | Iabstraction ->
      F.pp_print_string f "abstraction"
  | Iactual_precondition ->
      F.pp_print_string f "actual_precondition"
  | Ialloc ->
      F.pp_print_string f "alloc"
  | Iformal (zf, ncf) ->
      F.fprintf f "formal%a%a" pp_zero_flag zf pp_null_case_flag ncf
  | Iinitial ->
      F.pp_print_string f "initial"
  | Ilookup ->
      F.pp_print_string f "lookup"
  | Inone ->
      F.pp_print_string f "none"
  | Inullify ->
      F.pp_print_string f "nullify"
  | Irearrange (zf, ncf, n, _) ->
      F.fprintf f "rearrange:%a%a%d" pp_zero_flag zf pp_null_case_flag ncf n
  | Itaint ->
      F.pp_print_string f "taint"
  | Iupdate (zf, ncf, n, _) ->
      F.fprintf f "update:%a%a%d" pp_zero_flag zf pp_null_case_flag ncf n
  | Ireturn_from_call n ->
      F.fprintf f "return_from_call: %d" n


exception JoinFail

(** join of instrumentations, can raise JoinFail *)
let inst_partial_join inst1 inst2 =
  let fail () =
    L.d_printfln "inst_partial_join failed on %a %a" pp_inst inst1 pp_inst inst2 ;
    raise JoinFail
  in
  if equal_inst inst1 inst2 then inst1
  else
    match (inst1, inst2) with
    | _, Inone | Inone, _ ->
        inst_none
    | _, Ialloc | Ialloc, _ ->
        fail ()
    | _, Iinitial | Iinitial, _ ->
        fail ()
    | _, Iupdate _ | Iupdate _, _ ->
        fail ()
    | _ ->
        inst_none


(** meet of instrumentations *)
let inst_partial_meet inst1 inst2 = if equal_inst inst1 inst2 then inst1 else inst_none

(** Return the zero flag of the inst *)
let inst_zero_flag = function
  | Iabstraction ->
      None
  | Iactual_precondition ->
      None
  | Ialloc ->
      None
  | Iformal (zf, _) ->
      zf
  | Iinitial ->
      None
  | Ilookup ->
      None
  | Inone ->
      None
  | Inullify ->
      None
  | Irearrange (zf, _, _, _) ->
      zf
  | Itaint ->
      None
  | Iupdate (zf, _, _, _) ->
      zf
  | Ireturn_from_call _ ->
      None


(** Set the null case flag of the inst. *)
let inst_set_null_case_flag = function
  | Iformal (zf, false) ->
      Iformal (zf, true)
  | Irearrange (zf, false, n, pos) ->
      Irearrange (zf, true, n, pos)
  | Iupdate (zf, false, n, pos) ->
      Iupdate (zf, true, n, pos)
  | inst ->
      inst


(** Update [inst_old] to [inst_new] preserving the zero flag *)
let update_inst inst_old inst_new =
  let combine_zero_flags z1 z2 =
    match (z1, z2) with
    | Some b1, Some b2 ->
        Some (b1 || b2)
    | Some b, None ->
        Some b
    | None, Some b ->
        Some b
    | None, None ->
        None
  in
  match inst_new with
  | Iabstraction ->
      inst_new
  | Iactual_precondition ->
      inst_new
  | Ialloc ->
      inst_new
  | Iformal (zf, ncf) ->
      let zf' = combine_zero_flags (inst_zero_flag inst_old) zf in
      Iformal (zf', ncf)
  | Iinitial ->
      inst_new
  | Ilookup ->
      inst_new
  | Inone ->
      inst_new
  | Inullify ->
      inst_new
  | Irearrange (zf, ncf, n, pos) ->
      let zf' = combine_zero_flags (inst_zero_flag inst_old) zf in
      Irearrange (zf', ncf, n, pos)
  | Itaint ->
      inst_new
  | Iupdate (zf, ncf, n, pos) ->
      let zf' = combine_zero_flags (inst_zero_flag inst_old) zf in
      Iupdate (zf', ncf, n, pos)
  | Ireturn_from_call _ ->
      inst_new


(** describe an instrumentation with a string *)
let pp_inst_if_trace pe f inst =
  if Config.trace_error then
    if Pp.equal_print_kind pe.Pp.kind Pp.HTML then Io_infer.Html.with_color Orange pp_inst f inst
    else F.fprintf f "%s%a%s" (Binop.str pe Lt) pp_inst inst (Binop.str pe Gt)


(** pretty print a strexp with an optional predicate env *)
let rec pp_sexp_env pe0 envo f se =
  color_wrapper pe0 f se ~f:(fun pe f se ->
      match se with
      | Eexp (e, inst) ->
          F.fprintf f "%a%a" (pp_exp_printenv pe) e (pp_inst_if_trace pe) inst
      | Estruct (fel, inst) ->
          let pp_diff f (n, se) =
            F.fprintf f "%a:%a" Typ.Fieldname.pp n (pp_sexp_env pe envo) se
          in
          F.fprintf f "{%a}%a" (pp_seq_diff pp_diff pe) fel (pp_inst_if_trace pe) inst
      | Earray (len, nel, inst) ->
          let pp_diff f (i, se) =
            F.fprintf f "%a:%a" (pp_exp_printenv pe) i (pp_sexp_env pe envo) se
          in
          F.fprintf f "[%a|%a]%a" (pp_exp_printenv pe) len (pp_seq_diff pp_diff pe) nel
            (pp_inst_if_trace pe) inst )


(** Pretty print an hpred with an optional predicate env *)
let rec pp_hpred_env pe0 envo f hpred =
  color_wrapper pe0 f hpred ~f:(fun pe f hpred ->
      match hpred with
      | Hpointsto (e, se, te) ->
          let pe' =
            match (e, se) with
            | Lvar pvar, Eexp (Var _, _) when not (Pvar.is_global pvar) ->
                Pp.{pe with obj_sub= None} (* dont use obj sub on the var defining it *)
            | _ ->
                pe
          in
          F.fprintf f "%a|->%a:%a" (pp_exp_printenv pe') e (pp_sexp_env pe' envo) se
            (pp_texp_simple pe') te
      | Hlseg (k, hpara, e1, e2, elist) ->
          F.fprintf f "lseg%a(%a,%a,[%a],%a)" pp_lseg_kind k (pp_exp_printenv pe) e1
            (pp_exp_printenv pe) e2
            (Pp.comma_seq (pp_exp_printenv pe))
            elist (pp_hpara_env pe envo) hpara
      | Hdllseg (k, hpara_dll, iF, oB, oF, iB, elist) ->
          F.fprintf f "dllseg%a(%a,%a,%a,%a,[%a],%a)" pp_lseg_kind k (pp_exp_printenv pe) iF
            (pp_exp_printenv pe) oB (pp_exp_printenv pe) oF (pp_exp_printenv pe) iB
            (Pp.comma_seq (pp_exp_printenv pe))
            elist (pp_hpara_dll_env pe envo) hpara_dll )


and pp_hpara_env pe envo f hpara =
  match envo with
  | None ->
      let r, n, svars, evars, b = (hpara.root, hpara.next, hpara.svars, hpara.evars, hpara.body) in
      F.fprintf f "lam [%a,%a,%a]. exists [%a]. %a" Ident.pp r Ident.pp n (Pp.seq Ident.pp) svars
        (Pp.seq Ident.pp) evars
        (pp_star_seq (pp_hpred_env pe envo))
        b
  | Some env ->
      F.fprintf f "P%d" (Predicates.get_hpara_id env hpara)


and pp_hpara_dll_env pe envo f hpara_dll =
  match envo with
  | None ->
      let iF, oB, oF, svars, evars, b =
        ( hpara_dll.cell
        , hpara_dll.blink
        , hpara_dll.flink
        , hpara_dll.svars_dll
        , hpara_dll.evars_dll
        , hpara_dll.body_dll )
      in
      F.fprintf f "lam [%a,%a,%a,%a]. exists [%a]. %a" Ident.pp iF Ident.pp oB Ident.pp oF
        (Pp.seq Ident.pp) svars (Pp.seq Ident.pp) evars
        (pp_star_seq (pp_hpred_env pe envo))
        b
  | Some env ->
      F.fprintf f "P%d" (Predicates.get_hpara_dll_id env hpara_dll)


(** pretty print a strexp *)
let pp_sexp pe f = pp_sexp_env pe None f

(** pretty print a hpara *)
let pp_hpara pe f = pp_hpara_env pe None f

(** pretty print a hpara_dll *)
let pp_hpara_dll pe f = pp_hpara_dll_env pe None f

(** pretty print a hpred *)
let pp_hpred pe f = pp_hpred_env pe None f

(** dump a strexp. *)
let d_sexp (se : strexp) = L.d_pp_with_pe pp_sexp se

(** dump a hpred. *)
let d_hpred (hpred : hpred) = L.d_pp_with_pe pp_hpred hpred

(** {2 Functions for traversing SIL data types} *)

let rec strexp_expmap (f : Exp.t * inst option -> Exp.t * inst option) =
  let fe e = fst (f (e, None)) in
  let fei (e, inst) =
    match f (e, Some inst) with e', None -> (e', inst) | e', Some inst' -> (e', inst')
  in
  function
  | Eexp (e, inst) ->
      let e', inst' = fei (e, inst) in
      Eexp (e', inst')
  | Estruct (fld_se_list, inst) ->
      let f_fld_se (fld, se) = (fld, strexp_expmap f se) in
      Estruct (List.map ~f:f_fld_se fld_se_list, inst)
  | Earray (len, idx_se_list, inst) ->
      let len' = fe len in
      let f_idx_se (idx, se) =
        let idx' = fe idx in
        (idx', strexp_expmap f se)
      in
      Earray (len', List.map ~f:f_idx_se idx_se_list, inst)


let hpred_expmap (f : Exp.t * inst option -> Exp.t * inst option) =
  let fe e = fst (f (e, None)) in
  function
  | Hpointsto (e, se, te) ->
      let e' = fe e in
      let se' = strexp_expmap f se in
      let te' = fe te in
      Hpointsto (e', se', te')
  | Hlseg (k, hpara, root, next, shared) ->
      let root' = fe root in
      let next' = fe next in
      let shared' = List.map ~f:fe shared in
      Hlseg (k, hpara, root', next', shared')
  | Hdllseg (k, hpara, iF, oB, oF, iB, shared) ->
      let iF' = fe iF in
      let oB' = fe oB in
      let oF' = fe oF in
      let iB' = fe iB in
      let shared' = List.map ~f:fe shared in
      Hdllseg (k, hpara, iF', oB', oF', iB', shared')


let rec strexp_instmap (f : inst -> inst) strexp =
  match strexp with
  | Eexp (e, inst) ->
      Eexp (e, f inst)
  | Estruct (fld_se_list, inst) ->
      let f_fld_se (fld, se) = (fld, strexp_instmap f se) in
      Estruct (List.map ~f:f_fld_se fld_se_list, f inst)
  | Earray (len, idx_se_list, inst) ->
      let f_idx_se (idx, se) = (idx, strexp_instmap f se) in
      Earray (len, List.map ~f:f_idx_se idx_se_list, f inst)


let rec hpara_instmap (f : inst -> inst) hpara =
  {hpara with body= List.map ~f:(hpred_instmap f) hpara.body}


and hpara_dll_instmap (f : inst -> inst) hpara_dll =
  {hpara_dll with body_dll= List.map ~f:(hpred_instmap f) hpara_dll.body_dll}


and hpred_instmap (fn : inst -> inst) (hpred : hpred) : hpred =
  match hpred with
  | Hpointsto (e, se, te) ->
      let se' = strexp_instmap fn se in
      Hpointsto (e, se', te)
  | Hlseg (k, hpara, e, f, el) ->
      Hlseg (k, hpara_instmap fn hpara, e, f, el)
  | Hdllseg (k, hpar_dll, e, f, g, h, el) ->
      Hdllseg (k, hpara_dll_instmap fn hpar_dll, e, f, g, h, el)


let hpred_list_expmap (f : Exp.t * inst option -> Exp.t * inst option) (hlist : hpred list) =
  List.map ~f:(hpred_expmap f) hlist


let atom_expmap (f : Exp.t -> Exp.t) = function
  | Aeq (e1, e2) ->
      Aeq (f e1, f e2)
  | Aneq (e1, e2) ->
      Aneq (f e1, f e2)
  | Apred (a, es) ->
      Apred (a, List.map ~f es)
  | Anpred (a, es) ->
      Anpred (a, List.map ~f es)


(** {2 Function for computing lexps in sigma} *)

let hpred_get_lexp acc = function
  | Hpointsto (e, _, _) ->
      e :: acc
  | Hlseg (_, _, e, _, _) ->
      e :: acc
  | Hdllseg (_, _, e1, _, _, e2, _) ->
      e1 :: e2 :: acc


let hpred_list_get_lexps (filter : Exp.t -> bool) (hlist : hpred list) : Exp.t list =
  let lexps = List.fold ~f:hpred_get_lexp ~init:[] hlist in
  List.filter ~f:filter lexps


let hpred_entries hpred = hpred_get_lexp [] hpred

(** {2 Functions for computing free non-program variables} *)

let atom_gen_free_vars =
  let open Sequence.Generator in
  function
  | Aeq (e1, e2) | Aneq (e1, e2) ->
      Exp.gen_free_vars e1 >>= fun () -> Exp.gen_free_vars e2
  | Apred (_, es) | Anpred (_, es) ->
      ISequence.gen_sequence_list es ~f:Exp.gen_free_vars


let atom_free_vars a = Sequence.Generator.run (atom_gen_free_vars a)

let rec strexp_gen_free_vars =
  let open Sequence.Generator in
  function
  | Eexp (e, _) ->
      Exp.gen_free_vars e
  | Estruct (fld_se_list, _) ->
      ISequence.gen_sequence_list fld_se_list ~f:(fun (_, se) -> strexp_gen_free_vars se)
  | Earray (len, idx_se_list, _) ->
      Exp.gen_free_vars len
      >>= fun () ->
      ISequence.gen_sequence_list idx_se_list ~f:(fun (e, se) ->
          Exp.gen_free_vars e >>= fun () -> strexp_gen_free_vars se )


let hpred_gen_free_vars =
  let open Sequence.Generator in
  function
  | Hpointsto (base, sexp, te) ->
      Exp.gen_free_vars base
      >>= fun () -> strexp_gen_free_vars sexp >>= fun () -> Exp.gen_free_vars te
  | Hlseg (_, _, e1, e2, elist) ->
      Exp.gen_free_vars e1
      >>= fun () ->
      Exp.gen_free_vars e2 >>= fun () -> ISequence.gen_sequence_list elist ~f:Exp.gen_free_vars
  | Hdllseg (_, _, e1, e2, e3, e4, elist) ->
      Exp.gen_free_vars e1
      >>= fun () ->
      Exp.gen_free_vars e2
      >>= fun () ->
      Exp.gen_free_vars e3
      >>= fun () ->
      Exp.gen_free_vars e4 >>= fun () -> ISequence.gen_sequence_list elist ~f:Exp.gen_free_vars


let hpred_free_vars h = Sequence.Generator.run (hpred_gen_free_vars h)

(** {2 Functions for computing all free or bound non-program variables} *)

(** Variables in hpara, excluding bound vars in the body *)
let hpara_shallow_gen_free_vars {body; root; next; svars; evars} =
  let open Sequence.Generator in
  ISequence.gen_sequence_list ~f:hpred_gen_free_vars body
  >>= fun () ->
  yield root
  >>= fun () ->
  yield next
  >>= fun () ->
  ISequence.gen_sequence_list ~f:yield svars
  >>= fun () -> ISequence.gen_sequence_list ~f:yield evars


let hpara_shallow_free_vars h = Sequence.Generator.run (hpara_shallow_gen_free_vars h)

(** Variables in hpara_dll, excluding bound vars in the body *)
let hpara_dll_shallow_gen_free_vars {body_dll; cell; blink; flink; svars_dll; evars_dll} =
  let open Sequence.Generator in
  ISequence.gen_sequence_list ~f:hpred_gen_free_vars body_dll
  >>= fun () ->
  yield cell
  >>= fun () ->
  yield blink
  >>= fun () ->
  yield flink
  >>= fun () ->
  ISequence.gen_sequence_list ~f:yield svars_dll
  >>= fun () -> ISequence.gen_sequence_list ~f:yield evars_dll


let hpara_dll_shallow_free_vars h = Sequence.Generator.run (hpara_dll_shallow_gen_free_vars h)

(** {2 Functions for Substitution} *)

(** substitution *)
type ident_exp = Ident.t * Exp.t [@@deriving compare]

let compare_ident_exp_ids (id1, _) (id2, _) = Ident.compare id1 id2

type subst = ident_exp list [@@deriving compare]

type subst_fun = Ident.t -> Exp.t

let equal_subst = [%compare.equal: subst]

let sub_no_duplicated_ids sub = not (List.contains_dup ~compare:compare_ident_exp_ids sub)

(** Create a substitution from a list of pairs.
    For all (id1, e1), (id2, e2) in the input list,
    if id1 = id2, then e1 = e2. *)
let subst_of_list sub =
  let sub' = List.dedup_and_sort ~compare:compare_ident_exp sub in
  assert (sub_no_duplicated_ids sub') ;
  sub'


(** like subst_of_list, but allow duplicate ids and only keep the first occurrence *)
let subst_of_list_duplicates sub = List.dedup_and_sort ~compare:compare_ident_exp_ids sub

(** Convert a subst to a list of pairs. *)
let sub_to_list sub = sub

(** The empty substitution. *)
let sub_empty = subst_of_list []

let is_sub_empty = List.is_empty

(** Join two substitutions into one.
    For all id in dom(sub1) cap dom(sub2), sub1(id) = sub2(id). *)
let sub_join sub1 sub2 =
  let sub = IList.merge_dedup ~compare:compare_ident_exp sub1 sub2 in
  assert (sub_no_duplicated_ids sub) ;
  sub


(** Compute the common id-exp part of two inputs [subst1] and [subst2].
    The first component of the output is this common part.
    The second and third components are the remainder of [subst1]
    and [subst2], respectively. *)
let sub_symmetric_difference sub1_in sub2_in =
  let rec diff sub_common sub1_only sub2_only sub1 sub2 =
    match (sub1, sub2) with
    | [], _ | _, [] ->
        let sub1_only' = List.rev_append sub1_only sub1 in
        let sub2_only' = List.rev_append sub2_only sub2 in
        let sub_common = List.rev sub_common in
        (sub_common, sub1_only', sub2_only')
    | id_e1 :: sub1', id_e2 :: sub2' ->
        let n = compare_ident_exp id_e1 id_e2 in
        if Int.equal n 0 then diff (id_e1 :: sub_common) sub1_only sub2_only sub1' sub2'
        else if n < 0 then diff sub_common (id_e1 :: sub1_only) sub2_only sub1' sub2
        else diff sub_common sub1_only (id_e2 :: sub2_only) sub1 sub2'
  in
  diff [] [] [] sub1_in sub2_in


(** [sub_find filter sub] returns the expression associated to the first identifier
    that satisfies [filter]. Raise [Not_found] if there isn't one. *)
let sub_find filter (sub : subst) = snd (List.find_exn ~f:(fun (i, _) -> filter i) sub)

(** [sub_filter filter sub] restricts the domain of [sub] to the
    identifiers satisfying [filter]. *)
let sub_filter filter (sub : subst) = List.filter ~f:(fun (i, _) -> filter i) sub

(** [sub_filter_pair filter sub] restricts the domain of [sub] to the
    identifiers satisfying [filter(id, sub(id))]. *)
let sub_filter_pair = List.filter

(** [sub_range_partition filter sub] partitions [sub] according to
    whether range expressions satisfy [filter]. *)
let sub_range_partition filter (sub : subst) = List.partition_tf ~f:(fun (_, e) -> filter e) sub

(** [sub_domain_partition filter sub] partitions [sub] according to
    whether domain identifiers satisfy [filter]. *)
let sub_domain_partition filter (sub : subst) = List.partition_tf ~f:(fun (i, _) -> filter i) sub

(** Return the list of identifiers in the domain of the substitution. *)
let sub_domain sub = List.map ~f:fst sub

(** Return the list of expressions in the range of the substitution. *)
let sub_range sub = List.map ~f:snd sub

(** [sub_range_map f sub] applies [f] to the expressions in the range of [sub]. *)
let sub_range_map f sub = subst_of_list (List.map ~f:(fun (i, e) -> (i, f e)) sub)

(** [sub_map f g sub] applies the renaming [f] to identifiers in the domain
    of [sub] and the substitution [g] to the expressions in the range of [sub]. *)
let sub_map f g sub = subst_of_list (List.map ~f:(fun (i, e) -> (f i, g e)) sub)

let mem_sub id sub = List.exists ~f:(fun (id1, _) -> Ident.equal id id1) sub

(** Extend substitution and return [None] if not possible. *)
let extend_sub sub id exp : subst option =
  let compare (id1, _) (id2, _) = Ident.compare id1 id2 in
  if mem_sub id sub then None else Some (List.merge ~compare sub [(id, exp)])


(** Free auxilary variables in the domain and range of the substitution. *)
let subst_gen_free_vars sub =
  let open Sequence.Generator in
  ISequence.gen_sequence_list sub ~f:(fun (id, e) -> yield id >>= fun () -> Exp.gen_free_vars e)


let subst_free_vars sub = Sequence.Generator.run (subst_gen_free_vars sub)

let rec exp_sub_ids (f : subst_fun) exp =
  match (exp : Exp.t) with
  | Var id -> (
    match f id with
    | Exp.Var id' when Ident.equal id id' ->
        exp (* it will preserve physical equality when needed *)
    | exp' ->
        exp' )
  | Lvar _ ->
      exp
  | Exn e ->
      let e' = exp_sub_ids f e in
      if phys_equal e' e then exp else Exp.Exn e'
  | Closure c ->
      let captured_vars =
        IList.map_changed ~equal:[%compare.equal: Exp.t * Pvar.t * Typ.t]
          ~f:(fun ((e, pvar, typ) as captured) ->
            let e' = exp_sub_ids f e in
            if phys_equal e' e then captured else (e', pvar, typ) )
          c.captured_vars
      in
      if phys_equal captured_vars c.captured_vars then exp else Exp.Closure {c with captured_vars}
  | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _) ->
      exp
  | Cast (t, e) ->
      let e' = exp_sub_ids f e in
      if phys_equal e' e then exp else Exp.Cast (t, e')
  | UnOp (op, e, typ_opt) ->
      let e' = exp_sub_ids f e in
      if phys_equal e' e then exp else Exp.UnOp (op, e', typ_opt)
  | BinOp (op, e1, e2) ->
      let e1' = exp_sub_ids f e1 in
      let e2' = exp_sub_ids f e2 in
      if phys_equal e1' e1 && phys_equal e2' e2 then exp else Exp.BinOp (op, e1', e2')
  | Lfield (e, fld, typ) ->
      let e' = exp_sub_ids f e in
      if phys_equal e' e then exp else Exp.Lfield (e', fld, typ)
  | Lindex (e1, e2) ->
      let e1' = exp_sub_ids f e1 in
      let e2' = exp_sub_ids f e2 in
      if phys_equal e1' e1 && phys_equal e2' e2 then exp else Exp.Lindex (e1', e2')
  | Sizeof ({dynamic_length= Some l} as sizeof_data) ->
      let l' = exp_sub_ids f l in
      if phys_equal l' l then exp else Exp.Sizeof {sizeof_data with dynamic_length= Some l'}
  | Sizeof {dynamic_length= None} ->
      exp


let apply_sub subst : subst_fun =
 fun id ->
  match List.Assoc.find subst ~equal:Ident.equal id with Some x -> x | None -> Exp.Var id


let exp_sub (subst : subst) e = exp_sub_ids (apply_sub subst) e

(** apply [f] to id's in [instr]. if [sub_id_binders] is false, [f] is only applied to bound id's *)
let instr_sub_ids ~sub_id_binders f instr =
  let sub_id id =
    match exp_sub_ids f (Var id) with Var id' when not (Ident.equal id id') -> id' | _ -> id
  in
  match instr with
  | Load (id, rhs_exp, typ, loc) ->
      let id' = if sub_id_binders then sub_id id else id in
      let rhs_exp' = exp_sub_ids f rhs_exp in
      if phys_equal id' id && phys_equal rhs_exp' rhs_exp then instr
      else Load (id', rhs_exp', typ, loc)
  | Store (lhs_exp, typ, rhs_exp, loc) ->
      let lhs_exp' = exp_sub_ids f lhs_exp in
      let rhs_exp' = exp_sub_ids f rhs_exp in
      if phys_equal lhs_exp' lhs_exp && phys_equal rhs_exp' rhs_exp then instr
      else Store (lhs_exp', typ, rhs_exp', loc)
  | Call (((id, typ) as ret_id_typ), fun_exp, actuals, call_flags, loc) ->
      let ret_id' =
        if sub_id_binders then
          let id' = sub_id id in
          if Ident.equal id id' then ret_id_typ else (id', typ)
        else ret_id_typ
      in
      let fun_exp' = exp_sub_ids f fun_exp in
      let actuals' =
        IList.map_changed ~equal:[%compare.equal: Exp.t * Typ.t]
          ~f:(fun ((actual, typ) as actual_pair) ->
            let actual' = exp_sub_ids f actual in
            if phys_equal actual' actual then actual_pair else (actual', typ) )
          actuals
      in
      if
        phys_equal ret_id' ret_id_typ && phys_equal fun_exp' fun_exp && phys_equal actuals' actuals
      then instr
      else Call (ret_id', fun_exp', actuals', call_flags, loc)
  | Prune (exp, loc, true_branch, if_kind) ->
      let exp' = exp_sub_ids f exp in
      if phys_equal exp' exp then instr else Prune (exp', loc, true_branch, if_kind)
  | ExitScope (vars, loc) ->
      let sub_var var =
        match var with
        | Var.ProgramVar _ ->
            var
        | Var.LogicalVar ident ->
            let ident' = sub_id ident in
            if phys_equal ident ident' then var else Var.of_id ident'
      in
      let vars' = IList.map_changed ~equal:phys_equal ~f:sub_var vars in
      if phys_equal vars vars' then instr else ExitScope (vars', loc)
  | Nullify _ | Abstract _ ->
      instr


(** apply [subst] to all id's in [instr], including binder id's *)
let instr_sub (subst : subst) instr = instr_sub_ids ~sub_id_binders:true (apply_sub subst) instr

let atom_sub subst = atom_expmap (exp_sub subst)

let hpred_sub subst =
  let f (e, inst_opt) = (exp_sub subst e, inst_opt) in
  hpred_expmap f


(** {2 Functions for replacing occurrences of expressions.} *)

(** The first parameter should define a partial function.
    No parts of hpara are replaced by these functions. *)
let rec exp_replace_exp epairs e =
  (* First we check if there is an exact match *)
  match List.find ~f:(fun (e1, _) -> Exp.equal e e1) epairs with
  | Some (_, e2) ->
      e2
  | None -> (
    (* If e is a compound expression, we need to check for its subexpressions as well *)
    match e with
    | Exp.UnOp (op, e0, ty) ->
        let e0' = exp_replace_exp epairs e0 in
        if phys_equal e0 e0' then e else Exp.UnOp (op, e0', ty)
    | Exp.BinOp (op, lhs, rhs) ->
        let lhs' = exp_replace_exp epairs lhs in
        let rhs' = exp_replace_exp epairs rhs in
        if phys_equal lhs lhs' && phys_equal rhs rhs' then e else Exp.BinOp (op, lhs', rhs')
    | Exp.Cast (ty, e0) ->
        let e0' = exp_replace_exp epairs e0 in
        if phys_equal e0 e0' then e else Exp.Cast (ty, e0')
    | Exp.Lfield (e0, fname, ty) ->
        let e0' = exp_replace_exp epairs e0 in
        if phys_equal e0 e0' then e else Exp.Lfield (e0', fname, ty)
    | Exp.Lindex (base, index) ->
        let base' = exp_replace_exp epairs base in
        let index' = exp_replace_exp epairs index in
        if phys_equal base base' && phys_equal index index' then e else Exp.Lindex (base', index')
    | _ ->
        e )


let atom_replace_exp epairs atom = atom_expmap (fun e -> exp_replace_exp epairs e) atom

let rec strexp_replace_exp epairs = function
  | Eexp (e, inst) ->
      Eexp (exp_replace_exp epairs e, inst)
  | Estruct (fsel, inst) ->
      let f (fld, se) = (fld, strexp_replace_exp epairs se) in
      Estruct (List.map ~f fsel, inst)
  | Earray (len, isel, inst) ->
      let len' = exp_replace_exp epairs len in
      let f (idx, se) =
        let idx' = exp_replace_exp epairs idx in
        (idx', strexp_replace_exp epairs se)
      in
      Earray (len', List.map ~f isel, inst)


let hpred_replace_exp epairs = function
  | Hpointsto (root, se, te) ->
      let root_repl = exp_replace_exp epairs root in
      let strexp_repl = strexp_replace_exp epairs se in
      let te_repl = exp_replace_exp epairs te in
      Hpointsto (root_repl, strexp_repl, te_repl)
  | Hlseg (k, para, root, next, shared) ->
      let root_repl = exp_replace_exp epairs root in
      let next_repl = exp_replace_exp epairs next in
      let shared_repl = List.map ~f:(exp_replace_exp epairs) shared in
      Hlseg (k, para, root_repl, next_repl, shared_repl)
  | Hdllseg (k, para, e1, e2, e3, e4, shared) ->
      let e1' = exp_replace_exp epairs e1 in
      let e2' = exp_replace_exp epairs e2 in
      let e3' = exp_replace_exp epairs e3 in
      let e4' = exp_replace_exp epairs e4 in
      let shared_repl = List.map ~f:(exp_replace_exp epairs) shared in
      Hdllseg (k, para, e1', e2', e3', e4', shared_repl)


(** {2 Compaction} *)
module HpredInstHash = Hashtbl.Make (struct
  type t = hpred

  let equal = equal_hpred ~inst:true

  let hash = Hashtbl.hash
end)

type sharing_env = {exph: Exp.t Exp.Hash.t; hpredh: hpred HpredInstHash.t}

(** Create a sharing env to store canonical representations *)
let create_sharing_env () = {exph= Exp.Hash.create 3; hpredh= HpredInstHash.create 3}

(** Return a canonical representation of the exp *)
let exp_compact sh e =
  try Exp.Hash.find sh.exph e with Caml.Not_found -> Exp.Hash.add sh.exph e e ; e


let rec sexp_compact sh se =
  match se with
  | Eexp (e, inst) ->
      Eexp (exp_compact sh e, inst)
  | Estruct (fsel, inst) ->
      Estruct (List.map ~f:(fun (f, se) -> (f, sexp_compact sh se)) fsel, inst)
  | Earray _ ->
      se


(** Return a compact representation of the hpred *)
let hpred_compact_ sh hpred =
  match hpred with
  | Hpointsto (e1, se, e2) ->
      let e1' = exp_compact sh e1 in
      let e2' = exp_compact sh e2 in
      let se' = sexp_compact sh se in
      Hpointsto (e1', se', e2')
  | Hlseg _ ->
      hpred
  | Hdllseg _ ->
      hpred


let hpred_compact sh hpred =
  try HpredInstHash.find sh.hpredh hpred with Caml.Not_found ->
    let hpred' = hpred_compact_ sh hpred in
    HpredInstHash.add sh.hpredh hpred' hpred' ;
    hpred'


(** {2 Functions for constructing or destructing entities in this module} *)

(** Compute the offset list of an expression *)
let exp_get_offsets exp =
  let rec f offlist_past e =
    match (e : Exp.t) with
    | Var _
    | Const _
    | UnOp _
    | BinOp _
    | Exn _
    | Closure _
    | Lvar _
    | Sizeof {dynamic_length= None} ->
        offlist_past
    | Sizeof {dynamic_length= Some l} ->
        f offlist_past l
    | Cast (_, sub_exp) ->
        f offlist_past sub_exp
    | Lfield (sub_exp, fldname, typ) ->
        f (Off_fld (fldname, typ) :: offlist_past) sub_exp
    | Lindex (sub_exp, e) ->
        f (Off_index e :: offlist_past) sub_exp
  in
  f [] exp


let exp_add_offsets exp offsets =
  let rec f acc = function
    | [] ->
        acc
    | Off_fld (fld, typ) :: offs' ->
        f (Exp.Lfield (acc, fld, typ)) offs'
    | Off_index e :: offs' ->
        f (Exp.Lindex (acc, e)) offs'
  in
  f exp offsets


(** Convert all the lseg's in sigma to nonempty lsegs. *)
let sigma_to_sigma_ne sigma : (atom list * hpred list) list =
  if Config.nelseg then
    let f eqs_sigma_list hpred =
      match hpred with
      | Hpointsto _ | Hlseg (Lseg_NE, _, _, _, _) | Hdllseg (Lseg_NE, _, _, _, _, _, _) ->
          let g (eqs, sigma) = (eqs, hpred :: sigma) in
          List.map ~f:g eqs_sigma_list
      | Hlseg (Lseg_PE, para, e1, e2, el) ->
          let g (eqs, sigma) =
            [(Aeq (e1, e2) :: eqs, sigma); (eqs, Hlseg (Lseg_NE, para, e1, e2, el) :: sigma)]
          in
          List.concat_map ~f:g eqs_sigma_list
      | Hdllseg (Lseg_PE, para_dll, e1, e2, e3, e4, el) ->
          let g (eqs, sigma) =
            [ (Aeq (e1, e3) :: Aeq (e2, e4) :: eqs, sigma)
            ; (eqs, Hdllseg (Lseg_NE, para_dll, e1, e2, e3, e4, el) :: sigma) ]
          in
          List.concat_map ~f:g eqs_sigma_list
    in
    List.fold ~f ~init:[([], [])] sigma
  else [([], sigma)]


(** [hpara_instantiate para e1 e2 elist] instantiates [para] with [e1],
    [e2] and [elist]. If [para = lambda (x, y, xs). exists zs. b],
    then the result of the instantiation is [b\[e1 / x, e2 / y, elist / xs, zs'_/ zs\]]
    for some fresh [_zs'].*)
let hpara_instantiate para e1 e2 elist =
  let subst_for_svars =
    let g id e = (id, e) in
    try List.map2_exn ~f:g para.svars elist with Invalid_argument _ -> assert false
  in
  let ids_evars =
    let g _ = Ident.create_fresh Ident.kprimed in
    List.map ~f:g para.evars
  in
  let subst_for_evars =
    let g id id' = (id, Exp.Var id') in
    try List.map2_exn ~f:g para.evars ids_evars with Invalid_argument _ -> assert false
  in
  let subst =
    subst_of_list (((para.root, e1) :: (para.next, e2) :: subst_for_svars) @ subst_for_evars)
  in
  (ids_evars, List.map ~f:(hpred_sub subst) para.body)


(** [hpara_dll_instantiate para cell blink flink  elist] instantiates [para] with [cell],
    [blink], [flink], and [elist]. If [para = lambda (x, y, z, xs). exists zs. b],
    then the result of the instantiation is
    [b\[cell / x, blink / y, flink / z, elist / xs, zs'_/ zs\]]
    for some fresh [_zs'].*)
let hpara_dll_instantiate (para : hpara_dll) cell blink flink elist =
  let subst_for_svars =
    let g id e = (id, e) in
    try List.map2_exn ~f:g para.svars_dll elist with Invalid_argument _ -> assert false
  in
  let ids_evars =
    let g _ = Ident.create_fresh Ident.kprimed in
    List.map ~f:g para.evars_dll
  in
  let subst_for_evars =
    let g id id' = (id, Exp.Var id') in
    try List.map2_exn ~f:g para.evars_dll ids_evars with Invalid_argument _ -> assert false
  in
  let subst =
    subst_of_list
      ( ((para.cell, cell) :: (para.blink, blink) :: (para.flink, flink) :: subst_for_svars)
      @ subst_for_evars )
  in
  (ids_evars, List.map ~f:(hpred_sub subst) para.body_dll)


let custom_error = Pvar.mk_global (Mangled.from_string "INFER_CUSTOM_ERROR")
