(** We use big integers to store the integer values (this way we don't have to
    think about the bounds, nor architecture issues - Rust allows to manipulate
    128-bit integers for instance). *)
type big_int = (Z.t[@opaque])
[@@deriving
  visitors { name = "iter_big_int"; variety = "iter" },
  visitors { name = "map_big_int"; variety = "map" },
  visitors { name = "reduce_big_int"; variety = "reduce" },
  visitors { name = "mapreduce_big_int"; variety = "mapreduce" }]

let big_int_of_yojson (json : Yojson.Safe.t) : (big_int, string) result =
  match json with
  | `Int i -> Ok (Z.of_int i)
  | `Intlit is -> Ok (Z.of_string is)
  | _ -> Error "not an integer or an integer literal"

let big_int_to_yojson (i : big_int) = `Intlit (Z.to_string i)

let pp_big_int (fmt : Format.formatter) (bi : big_int) : unit =
  Format.pp_print_string fmt (Z.to_string bi)

let compare_big_int (bi0 : big_int) (bi1 : big_int) : int = Z.compare bi0 bi1
let show_big_int (bi : big_int) : string = Z.to_string bi
let equal_big_int (bi0 : big_int) (bi1 : big_int) : bool = Z.equal bi0 bi1
