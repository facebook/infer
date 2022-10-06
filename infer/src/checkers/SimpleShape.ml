(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module CFG = ProcCfg.NormalOneInstrPerNode

(** This analysis aims at inferring all the fields that may be accessed from some variable, possibly
    through aliases.

    The original goal is to be able to use that information in the lineage analysis, by making
    information flow through fields even if they appear in later instructions. The typical use-case
    is when an argument [$argN] of a function is copied into the name [X], and then the function
    returns some field [foo] of [X]: we want to know that we should create the Lineage node
    [$argN#foo] at the same time as the node [$argN]. *)

module Shape : sig
  (** A module that manages shape definitions and environments. This is mostly done through a
      stateful API to ease the implementation that currently heavily relies on a union-find data
      structure (which is easier to implement imperatively). *)

  (** A shape can be seen as a handle to a simplified version of a type. To this handle will be
      attached the subscriptable fields of the variables/fields that have this type. *)
  type t

  (** An environment associates shapes to variables and fields to shapes. *)
  type env

  val pp_env : Format.formatter -> env -> unit

  val create : unit -> t
  (** Create a fresh shape, not associated to any variable yet, and having no known field. *)

  val create_env : unit -> env
  (** Create a fresh environment with no known variable nor shape fields. *)

  val of_var : Var.t -> env -> t
  (** Returns the shape of a variable. If the variable is unknown in the environment, a fresh shape
      will be created, associated to the variable, and returned.

      Will always return a fresh shape for the `_` anonymous erlang variable. *)

  val of_field : t -> Fieldname.t -> env -> t
  (** Given a shape, a subscripted field and an environment, returns the shape of the field content.
      If this field is unknown for that shape in the environment, a fresh shape will be created,
      associated to the field and returned. *)

  val unify : t -> t -> env -> unit
  (** Makes two shapes identical by merging their defined fields (unifying the shapes of common
      fields if needed) and making it so that they will both share the same identical set of fields
      in the future (one can now indifferently refer to any of the shapes as an alias of the
      result). *)
end = struct
  (** A shape Id is what links variables to defined fields. It is a unique identifier to which a set
      of fields is associated, and that will be indirectly assigned to every variable. *)
  module Id = Unique_id.Int63 ()

  (** A shape is an equivalence class of shape identifiers, that is, shape identifiers that share a
      common determined set of fields. *)
  type t = Id.t Union_find.t

  let pp_id fmt x = Format.fprintf fmt "<%d>" (Id.to_int_exn x)

  let sexp_of_t x = [%sexp_of: _] (Union_find.get x)

  type fields = (Fieldname.t, t) Hashtbl.t [@@deriving sexp_of]

  (** An environment associates to each variable its equivalence class, and to each possible
      representant of an equivalence class the set of known fields. *)
  type env = {var_shape: (Var.t, t) Hashtbl.t; shape_fields: (Id.t, fields) Hashtbl.t}

  let create_env () =
    {var_shape= Hashtbl.create (module Var); shape_fields= Hashtbl.create (module Id)}


  let pp_hashtbl pp_key pp_value fmt hashtbl =
    let pp_binding fmt (key, value) = Format.fprintf fmt "@[%a -> %a@]" pp_key key pp_value value in
    Format.fprintf fmt "@[(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp_binding)
      (Hashtbl.to_alist hashtbl)


  let pp_shape fmt x = pp_id fmt (Union_find.get x)

  let pp_env fmt {var_shape; shape_fields} =
    Format.fprintf fmt "@[<v>@[<v4>VAR_SHAPE@;@[%a@]@]@;@[<v4>SHAPE_FIELDS@;@[%a@]@]@]@;"
      (pp_hashtbl Var.pp pp_shape) var_shape
      (pp_hashtbl pp_id (pp_hashtbl Fieldname.pp pp_shape))
      shape_fields


  let create () =
    let id = Id.create () in
    Union_find.create id


  let of_var var {var_shape; _} =
    (* TODO T132509349 -- Hopefully temporary hack: always use fresh (empty) shapes for the anonymous
       variable (otherwise it could lead to spurious unifications between unrelated uses of this
       special variable, and transitively between actual variables). *)
    if String.(Format.asprintf "%a" Var.pp var = "_") then create ()
    else Hashtbl.find_or_add ~default:create var_shape var


  let of_field shape fieldname {shape_fields; _} =
    (* Proceed in two steps: retrieve the field set of this shape or create it, then return the shape
       of the asked field or create it. *)
    let id = Union_find.get shape in
    let field_table =
      Hashtbl.find_or_add ~default:(fun () -> Hashtbl.create (module Fieldname)) shape_fields id
    in
    Hashtbl.find_or_add ~default:create field_table fieldname


  let rec unify shape shape' ({shape_fields; _} as env) =
    (* We need to explicitly check that we are not trying to unify already unified classes to ensure
       termination. Otherwise, given a recursive shape such as [<0> -> { tail : <0> }], unifying
       [ <0> ] with itself would recursively try to unify its fields, therefore recursively
       proceeding to unify [ <0> ] with itself and so on.

       This check guarantees termination because shape unification can now only strictly reduce the
       number of equivalence classes, which is finite. *)
    if Union_find.same_class shape shape' then ()
    else
      let id = Union_find.get shape in
      let id' = Union_find.get shape' in
      let () = Union_find.union shape shape' in
      let new_id = Union_find.get shape in
      let fields = Hashtbl.find shape_fields id in
      let fields' = Hashtbl.find shape_fields id' in
      let new_fields =
        match (fields, fields') with
        | None, None ->
            Hashtbl.create (module Fieldname)
        | Some f, None | None, Some f ->
            f
        | Some fields, Some fields' ->
            unify_fields fields fields' env
      in
      (* Should we remove old shape entries? This could potentially reduce the memory footprint but
         preliminary experiments seem to show that it is not needed (and extra care has to be taken to not
         remove it before trying to access its definition). *)
      Hashtbl.set shape_fields ~key:new_id ~data:new_fields


  and unify_fields fields fields' env =
    Hashtbl.merge
      ~f:(fun ~key:_ values ->
        match values with
        | `Left shape | `Right shape ->
            Some shape
        | `Both (shape, shape') ->
            unify shape shape' env ;
            Some shape )
      fields fields'
end

(** As the environment works imperatively, we do not need to propagate it and therefore use a simple
    Top/Bottom abstract domain that will simply remember which states have been reached yet. Note
    that the resulting analysis is flow insensitive, as the same global shape environment will be
    augmented during the traversal of a procedure code, and no intermediary version of it will be
    associated to the individual control flow vertices. *)
module Domain = AbstractDomain.Unit

module Report = struct
  (** A utility module for use while developing. This would likely be integrated within a proper
      Summary one when interprocedural analysis is supported. *)

  let debug result proc_desc =
    (* For now we just want to print environment somewhere to debug the analysis. *)
    let procname = Procdesc.get_proc_name proc_desc in
    L.debug Analysis Verbose "@[<v>Result for procedure : %a@,@]" Procname.pp procname ;
    L.debug Analysis Verbose "@[<v>%a@,@]" Shape.pp_env result
end

(** Transfer functions to compute shapes. As the environment is an imperative structure, this module
    takes a global environment as a parameter that will be mutated through the analysis by the
    hereby defined transfer functions. *)
module TransferFunctions (Env : sig
  val env : Shape.env
end) =
struct
  module Domain = Domain
  module CFG = CFG

  type analysis_data = IntraproceduralAnalysis.t

  let env = Env.env

  (** Returns the shape of an expression. Fresh shapes will be created as needed. *)
  let rec shape_expr (e : Exp.t) =
    match e with
    | Const _ | Closure _ ->
        (* We use fresh ids to represent shapes that do not hold fields. *)
        Shape.create ()
    | Var id ->
        Shape.of_var (Var.of_id id) env
    | Lvar pvar ->
        Shape.of_var (Var.of_pvar pvar) env
    | Lfield (e, fieldname, _) ->
        let shape_e = shape_expr e in
        Shape.of_field shape_e fieldname env
    | Sizeof {dynamic_length= None} ->
        Shape.create ()
    | UnOp (_, e, _) | Exn e | Cast (_, e) | Sizeof {dynamic_length= Some e} ->
        (* We first shape [e] to possibly discover some fields (eg. on [not (x.f)]), then return
           a fresh id as unary operators only return scalar value. *)
        ignore (shape_expr e : Shape.t) ;
        Shape.create ()
    | BinOp (_, e1, e2) | Lindex (e1, e2) ->
        (* Similar to the UnOp case *)
        ignore (shape_expr e1 : Shape.t) ;
        ignore (shape_expr e2 : Shape.t) ;
        Shape.create ()


  let exec_assignment var rhs_exp =
    (* When assigning a value to a variable, we unify the current shape of that variable to the shape
       of the expression, thus merging together the fields that have been collected so far on both
       sides.

       Note that this might lead to over-approximating the field set of a variable that would be
       reassigned in the program with completely unrelated types. We believe that it does not happen
       with the Erlang translation anyway (and even then would not be a fundamental issue). *)
    let var_shape = Shape.of_var var env in
    let expr_shape = shape_expr rhs_exp in
    Shape.unify var_shape expr_shape env


  let procname_of_exp (e : Exp.t) : Procname.t option =
    match e with Closure {name} | Const (Cfun name) -> Some name | _ -> None


  (** Execute an instruction by mutating the environment *)
  let exec_instr_unit _ (* {InterproceduralAnalysis.analyze_dependency} *) (instr : Sil.instr) =
    match instr with
    | Call ((_ret_id, _typ), fun_exp, _args, _location, _flags) ->
        L.debug Analysis Verbose
          "@[<v 2>SimpleShape: call detected (warning: not fully supported).@,\
           Procedure: %a@,\
           Procname: %a@]@,"
          Exp.pp fun_exp
          (Format.pp_print_option Procname.pp)
          (procname_of_exp fun_exp)
    | Prune (e, _, _, _) ->
        ignore (shape_expr e : Shape.t)
    | Metadata _ ->
        ()
    | Load {id; e; _} ->
        exec_assignment (Var.of_id id) e
    | Store {e1= Lvar pvar; e2; _} ->
        (* Same as Load *)
        exec_assignment (Var.of_pvar pvar) e2
    | Store _ ->
        L.die InternalError "SimpleShape: Store instructions are only supported with Lvar lhs"


  (** Mutates the environment and then return an abstract state (which is actually the same as the
      parameter). *)
  let exec_instr astate interproc_data _node _instr_index instr =
    let () = exec_instr_unit interproc_data instr in
    astate


  let pp_session_name _node fmt = Format.pp_print_string fmt "SimpleLineageShape"
end

(** A generative module that creates a fresh environment and passes it to the {!TransferFunctions}
    functor to build an analysis engine. *)
module Analyzer () = struct
  module Env = struct
    let env = Shape.create_env ()
  end

  include AbstractInterpreter.MakeRPO (TransferFunctions (Env))
end

let unskipped_checker ({IntraproceduralAnalysis.proc_desc} as analysis_data) =
  let module Analyzer = Analyzer () in
  let _invmap : Analyzer.invariant_map = Analyzer.exec_pdesc analysis_data ~initial:() proc_desc in
  (* Just print the final environment in the debug logs for now *)
  Report.debug Analyzer.Env.env proc_desc ;
  ()


let checker =
  (* TODO when going interprocedural: Do not analyze the functions that will not be analyzed by
     Lineage anyway (this is cumbersome to do with SimpleLineageUtils on the intraproc draft for
     typing issues) *)
  unskipped_checker
