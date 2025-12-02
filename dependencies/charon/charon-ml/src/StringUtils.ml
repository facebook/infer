(** Utilities to work on strings, character per character.

    They operate on ASCII strings, and are used by the project to convert Rust
    names: Rust names are not fancy, so it shouldn't be a problem.

    TODO: isn't there a library with good support for strings, somewhere? *)

let code_0 = 48
let code_9 = 57
let code_A = 65
let code_Z = 90
let code_a = 97
let code_z = 122

let is_lowercase_ascii (c : char) : bool =
  let c = Char.code c in
  code_a <= c && c <= code_z

let is_uppercase_ascii (c : char) : bool =
  let c = Char.code c in
  code_A <= c && c <= code_Z

let is_letter_ascii (c : char) : bool =
  is_lowercase_ascii c || is_uppercase_ascii c

let is_digit_ascii (c : char) : bool =
  let c = Char.code c in
  code_0 <= c && c <= code_9

let lowercase_ascii = Char.lowercase_ascii
let uppercase_ascii = Char.uppercase_ascii

(** Using buffers as per:
    {{:https://stackoverflow.com/questions/29957418/how-to-convert-char-list-to-string-in-ocaml}
     stackoverflow} *)
let string_of_chars (chars : char list) : string =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let string_to_chars (s : string) : char list =
  let length = String.length s in
  let rec apply i =
    if i = length then [] else String.get s i :: apply (i + 1)
  in
  apply 0

(** This operates on ASCII *)
let to_camel_case (s : string) : string =
  (* Note that we rebuild the string in reverse order *)
  let apply ((prev_is_under, acc) : bool * char list) (c : char) :
      bool * char list =
    if c = '_' then (true, acc)
    else
      let c = if prev_is_under then uppercase_ascii c else c in
      (false, c :: acc)
  in
  let _, chars = List.fold_left apply (true, []) (string_to_chars s) in
  string_of_chars (List.rev chars)

(** This operates on ASCII *)
let to_snake_case (s : string) : string =
  (* Note that we rebuild the string in reverse order *)
  let apply ((prev_is_low, prev_is_digit, acc) : bool * bool * char list)
      (c : char) : bool * bool * char list =
    (* Note that we have a special treatment for the character "'", which
       we treat as a lowercase letter.
       This character is sometimes used for backward functions because of
       lifetime names *)
    let is_special c = c = '\'' in
    let acc =
      if c = '_' then acc
      else if prev_is_digit then if is_letter_ascii c then '_' :: acc else acc
      else if prev_is_low then
        if
          (is_lowercase_ascii c || is_digit_ascii c || is_special c) && c <> '_'
        then acc
        else '_' :: acc
      else acc
    in
    let prev_is_low = is_lowercase_ascii c || is_special c in
    let prev_is_digit = is_digit_ascii c in
    let c = lowercase_ascii c in
    (prev_is_low, prev_is_digit, c :: acc)
  in
  let _, _, chars =
    List.fold_left apply (false, false, []) (string_to_chars s)
  in
  string_of_chars (List.rev chars)

(** Applies a map operation.

    This is very inefficient, but shouldn't be used much. *)
let map (f : char -> string) (s : string) : string =
  let sl = List.map f (string_to_chars s) in
  let sl = List.map string_to_chars sl in
  string_of_chars (List.concat sl)

let capitalize_first_letter (s : string) : string =
  let s = string_to_chars s in
  let s =
    match s with
    | [] -> s
    | c :: s' -> uppercase_ascii c :: s'
  in
  string_of_chars s

let lowercase_first_letter (s : string) : string =
  let s = string_to_chars s in
  let s =
    match s with
    | [] -> s
    | c :: s' -> lowercase_ascii c :: s'
  in
  string_of_chars s

(** Unit tests *)
let _ =
  assert (to_camel_case "hello_world" = "HelloWorld");
  assert (to_snake_case "HelloWorld36Hello" = "hello_world36_hello");
  assert (to_snake_case "HELLO" = "hello");
  assert (to_snake_case "T1" = "t1");
  assert (to_camel_case "list" = "List");
  assert (to_snake_case "is_cons" = "is_cons")
