(* TODO: this duplicates PrimitiveValues *)
type big_int = Z.t

let pp_big_int (fmt : Format.formatter) (bi : big_int) : unit =
  Format.pp_print_string fmt (Z.to_string bi)

let compare_big_int (bi0 : big_int) (bi1 : big_int) : int = Z.compare bi0 bi1
let show_big_int (bi : big_int) : string = Z.to_string bi

(** Ancestor the pattern iter visitor *)
class ['self] iter_pattern_base =
  object (_self : 'self)
    inherit [_] VisitorsRuntime.iter
    method visit_big_int : 'env -> big_int -> unit = fun _ _ -> ()
  end

(** Ancestor the pattern map visitor *)
class ['self] map_pattern_base =
  object (_self : 'self)
    inherit [_] VisitorsRuntime.map
    method visit_big_int : 'env -> big_int -> big_int = fun _ x -> x
  end

type var = VarName of string | VarIndex of int
and literal = LInt of big_int | LBool of bool | LChar of char
and ref_kind = RMut | RShared
and region = RVar of var option | RStatic
and primitive_adt = TTuple | TArray | TSlice
and mutability = Mut | Not
and pattern = pattern_elem list

and pattern_elem =
  | PIdent of string * int * generic_args
      (** The integer is a disambiguator

          Generally speaking, we need to preserve the disambiguator if it is non
          zero.

          For instance, it is possible to define const values with the same
          names in different branches of a match. In this case, we have to use
          the disambiguator to generate unique names for each const:
          {[
            match ... {
              Variant1 => {
                const N_ROWS:usize = 4;
                ...
              }
              Variant2 => {
                const N_ROWS:usize = 4;
                ...
              }
            }
          ]}

          In order to not use disambiguators everywhere in patterns, we allow
          omitting the disambiguator when it is equal to 0. *)
  | PImpl of expr
  | PWild  (** A wildcard pattern, that matches anything *)

(** An expression can be a type or a trait ref.

    Note that we put in separate cases the tuple, array, slice and reference
    types because they have special syntax. *)
and expr =
  | EComp of pattern
      (** Compound expression: instantiated adt, primitive type, constant, etc.
          Note that if a type has generic arguments, they will be grouped with
          the last pattern elem. *)
  | EPrimAdt of primitive_adt * generic_args
  | ERef of region * expr * ref_kind
  | EArrow of expr list * expr option
  | EVar of var option
  | ERawPtr of mutability * expr

and generic_arg = GExpr of expr | GValue of literal | GRegion of region

and generic_args = generic_arg list
[@@deriving
  show,
  ord,
  visitors
    {
      name = "iter_pattern";
      variety = "iter";
      ancestors = [ "iter_pattern_base" ];
      nude = true (* Don't inherit {!VisitorsRuntime.iter} *);
      concrete = true;
      polymorphic = false;
    },
  visitors
    {
      name = "map_pattern";
      variety = "map";
      ancestors = [ "map_pattern_base" ];
      nude = true (* Don't inherit {!VisitorsRuntime.map} *);
      concrete = false;
      polymorphic = false;
    }]
