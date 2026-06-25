type enum = A | B of int | C of bool * int | D of { a : int; b : string }
  [@@deriving show]

let exit_code = ref 0

let string_match line s s' =
  if s <> s' then
    begin
      Format.eprintf "Mismatch at line %d: got \"%s\" but \"%s\" expected@."
        line (String.escaped s) (String.escaped s');
      exit_code := 1
    end

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int32] 1l) "1l"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int64] 1L) "1L"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: nativeint] 1n) "1n"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: float] 1.) "1."

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int option] (Some 1)) "Some (1)"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int ref] (ref 1)) "ref (1)"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int Lazy.t] (lazy 1)) "lazy (1)"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: (int,  unit) result] (Error ()))
    "Error (())"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int list] [1; 2; 3]) "[1; 2; 3]"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int * bool * string] (1, false, "a"))
    "(1, false, \"a\")"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: [`A | `B of int]] `A) "`A"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: [`A | `B of int]] (`B 1)) "`B (1)"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: [`A | `B of int * int]] (`B (1, 2)))
    "`B ((1, 2))"

let () =
  string_match __LINE__ (Format.asprintf "@[%a@]" pp_enum A) "Show.A"

let () =
  string_match __LINE__ (Format.asprintf "@[%a@]" pp_enum (B 1)) "Show.B (1)"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" pp_enum (C (false, 2))) "Show.C (false, 2)"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" pp_enum (D { a = 1; b = "foo" }))
    "Show.D ({ a = 1; b = \"foo\" })"

type 'a poly = A of enum | B of 'a poly * 'a
  [@@deriving show { with_path = false }]

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" (pp_poly pp_enum) (A A)) "A (Show.A)"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" (pp_poly pp_enum) (B (A A, A)))
      "B (A (Show.A), Show.A)"

let pp_int fmt _ = Format.pp_print_string fmt "a"

let () =
  string_match __LINE__
    (Format.asprintf "@[%a@]" [%show: int [@show.nobuiltin]] 1) "a"

module Test : sig
  type t = A [@@deriving show]
end = struct
  type t = A [@@deriving show]
end

let () =
  string_match __LINE__ (Test.show A) "Show.Test.A"

let () = exit !exit_code
