(** Pretty-printing for primitive values *)

open Values
open Types

let integer_type_to_string = function
  | Signed Isize -> "isize"
  | Signed I8 -> "i8"
  | Signed I16 -> "i16"
  | Signed I32 -> "i32"
  | Signed I64 -> "i64"
  | Signed I128 -> "i128"
  | Unsigned Usize -> "usize"
  | Unsigned U8 -> "u8"
  | Unsigned U16 -> "u16"
  | Unsigned U32 -> "u32"
  | Unsigned U64 -> "u64"
  | Unsigned U128 -> "u128"

let float_type_to_string = function
  | F16 -> "f16"
  | F32 -> "f32"
  | F64 -> "f64"
  | F128 -> "f128"

let literal_type_to_string (ty : literal_type) : string =
  match ty with
  | TInt ity -> integer_type_to_string (Signed ity)
  | TUInt uty -> integer_type_to_string (Unsigned uty)
  | TFloat fty -> float_type_to_string fty
  | TBool -> "bool"
  | TChar -> "char"

let big_int_to_string (bi : big_int) : string = Z.to_string bi

let scalar_value_to_string (sv : scalar_value) : string =
  big_int_to_string (Scalars.get_val sv)
  ^ integer_type_to_string (Scalars.get_ty sv)

let float_value_to_string (fv : float_value) : string =
  fv.float_value ^ float_type_to_string fv.float_ty

let literal_to_string (lit : literal) : string =
  match lit with
  | VScalar sv -> scalar_value_to_string sv
  | VFloat fv -> float_value_to_string fv
  | VBool b -> Bool.to_string b
  | VChar c -> Uchar.to_string c
  | VStr s -> "\"" ^ s ^ "\""
  | VByteStr bs -> "[" ^ String.concat ", " (List.map string_of_int bs) ^ "]"
