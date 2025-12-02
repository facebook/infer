(** The primitive values. *)
include BigInt

include Uchar

type float_type = F16 | F32 | F64 | F128

(** This is simlar to the Scalar value above. However, instead of storing the
    float value itself, we store its String representation. This allows to
    derive the Eq and Ord traits, which are not implemented for floats *)
and float_value = { float_value : string; float_ty : float_type }

and int_ty = Isize | I8 | I16 | I32 | I64 | I128
and integer_type = Signed of int_ty | Unsigned of u_int_ty

(** A primitive value.

    Those are for instance used for the constant operands
    [crate::expressions::Operand::Const] *)
and literal =
  | VScalar of scalar_value
  | VFloat of float_value
  | VBool of bool
  | VChar of (Uchar.t[@visitors.opaque])
  | VByteStr of int list
  | VStr of string

(** Types of primitive values. Either an integer, bool, char *)
and literal_type =
  | TInt of int_ty
  | TUInt of u_int_ty
  | TFloat of float_type
  | TBool
  | TChar

(** A scalar value. *)
and scalar_value =
  | UnsignedScalar of u_int_ty * big_int
  | SignedScalar of int_ty * big_int

and u_int_ty = Usize | U8 | U16 | U32 | U64 | U128
[@@deriving
  show,
  eq,
  ord,
  visitors
    {
      name = "iter_literal";
      monomorphic = [ "env" ];
      variety = "iter";
      ancestors = [ "iter_big_int" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "map_literal";
      monomorphic = [ "env" ];
      variety = "map";
      ancestors = [ "map_big_int" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "reduce_literal";
      monomorphic = [ "env" ];
      variety = "reduce";
      ancestors = [ "reduce_big_int" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    },
  visitors
    {
      name = "mapreduce_literal";
      monomorphic = [ "env" ];
      variety = "mapreduce";
      ancestors = [ "mapreduce_big_int" ];
      nude = true (* Don't inherit VisitorsRuntime *);
    }]
