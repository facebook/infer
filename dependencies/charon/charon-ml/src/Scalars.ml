open Values

(** The minimum/maximum values an integer type can have depending on its type *)

let i8_min = Z.of_string "-128"
let i8_max = Z.of_string "127"
let i16_min = Z.of_string "-32768"
let i16_max = Z.of_string "32767"
let i32_min = Z.of_string "-2147483648"
let i32_max = Z.of_string "2147483647"
let i64_min = Z.of_string "-9223372036854775808"
let i64_max = Z.of_string "9223372036854775807"
let i128_min = Z.of_string "-170141183460469231731687303715884105728"
let i128_max = Z.of_string "170141183460469231731687303715884105727"
let u8_min = Z.of_string "0"
let u8_max = Z.of_string "255"
let u16_min = Z.of_string "0"
let u16_max = Z.of_string "65535"
let u32_min = Z.of_string "0"
let u32_max = Z.of_string "4294967295"
let u64_min = Z.of_string "0"
let u64_max = Z.of_string "18446744073709551615"
let u128_min = Z.of_string "0"
let u128_max = Z.of_string "340282366920938463463374607431768211455"

(** The bounds for isize and usize vary with the architecture.

    Note that we use those bounds only to check that the values are *in range*
    when:
    - deserializing
    - using the interpreter in *concrete* mode and evaluating operations like
      addition, negation, etc. It is thus ok to not use the precise bounds. *)

let isize_min ptr_size =
  match ptr_size with
  | 8 -> i64_min
  | 4 -> i32_min
  | 2 -> i16_min
  | _ -> raise (Failure "Unsupported target pointer size")

let isize_max ptr_size =
  match ptr_size with
  | 8 -> i64_max
  | 4 -> i32_max
  | 2 -> i16_max
  | _ -> raise (Failure "Unsupported target pointer size")

let usize_min ptr_size =
  match ptr_size with
  | 8 -> u64_min
  | 4 -> u32_min
  | 2 -> u16_min
  | _ -> raise (Failure "Unsupported target pointer size")

let usize_max ptr_size =
  match ptr_size with
  | 8 -> u64_max
  | 4 -> u32_max
  | 2 -> u16_max
  | _ -> raise (Failure "Unsupported target pointer size")

let scalar_min ptr_size (int_ty : integer_type) : big_int =
  match int_ty with
  | Signed Isize -> isize_min ptr_size
  | Signed I8 -> i8_min
  | Signed I16 -> i16_min
  | Signed I32 -> i32_min
  | Signed I64 -> i64_min
  | Signed I128 -> i128_min
  | Unsigned Usize -> usize_min ptr_size
  | Unsigned U8 -> u8_min
  | Unsigned U16 -> u16_min
  | Unsigned U32 -> u32_min
  | Unsigned U64 -> u64_min
  | Unsigned U128 -> u128_min

let scalar_max ptr_size (int_ty : integer_type) : big_int =
  match int_ty with
  | Signed Isize -> isize_max ptr_size
  | Signed I8 -> i8_max
  | Signed I16 -> i16_max
  | Signed I32 -> i32_max
  | Signed I64 -> i64_max
  | Signed I128 -> i128_max
  | Unsigned Usize -> usize_max ptr_size
  | Unsigned U8 -> u8_max
  | Unsigned U16 -> u16_max
  | Unsigned U32 -> u32_max
  | Unsigned U64 -> u64_max
  | Unsigned U128 -> u128_max

(** Check that an integer value is in range *)
let check_int_in_range ptr_size (int_ty : integer_type) (i : big_int) : bool =
  Z.leq (scalar_min ptr_size int_ty) i && Z.leq i (scalar_max ptr_size int_ty)

let get_val (scalar : scalar_value) =
  match scalar with
  | SignedScalar (_, v) | UnsignedScalar (_, v) -> v

let get_ty (scalar : scalar_value) =
  match scalar with
  | SignedScalar (int_ty, _) -> Signed int_ty
  | UnsignedScalar (uint_ty, _) -> Unsigned uint_ty

(** Check that a scalar value is correct (the integer value it contains is in
    range) *)
let check_scalar_value_in_range ptr_size (v : scalar_value) : bool =
  check_int_in_range ptr_size (get_ty v) (get_val v)

(** Make a scalar value, while checking the value is in range *)
let mk_scalar ptr_size (int_ty : integer_type) (i : big_int) :
    (scalar_value, unit) result =
  if check_int_in_range ptr_size int_ty i then
    Ok
      (match int_ty with
      | Signed int_ty -> SignedScalar (int_ty, i)
      | Unsigned uint_ty -> UnsignedScalar (uint_ty, i))
  else Error ()

let integer_type_is_signed (int_ty : integer_type) : bool =
  match int_ty with
  | Signed _ -> true
  | Unsigned _ -> false
