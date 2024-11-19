module Option = struct
  type 'a t = 'a option

  let map f o =
    match o with
    | None -> None
    | Some x -> Some (f x)

  let equal p o o' =
    match o, o' with
    | None, None -> true
    | Some x, Some y -> p x y
    | Some _, None
    | None, Some _ -> false

  let exists p o =
    match o with
    | None -> false
    | Some x -> p x

  let some x = Some x

  let iter f o =
    match o with
    | None -> ()
    | Some x -> f x

  let filter p o =
    match o with
    | Some x when p x -> o
    | None | Some _ -> None
end

module Version = struct
  type t = {
      major : int;
      minor : int;
      patch : int;
    }

  let mk major minor patch =
    { major; minor; patch }

  let compare (v : t) (v' : t) =
    compare v v'

  let equal (v : t) (v' : t) =
    v = v'

  let hash (v : t) =
    Hashtbl.hash v

  let of_string version_line =
    let index =
      match String.rindex version_line ' ' with
      | space_index -> space_index + 1
      | exception Not_found -> 0 in
    let version =
      String.sub version_line index (String.length version_line - index) in
    let major, minor =
      match String.split_on_char '.' version with
      | [major; minor; _patch] -> major, minor
      | [major; minor] -> major, minor
      | _ -> assert false in
    { major = int_of_string major;
      minor = int_of_string minor;
      patch = 0; }

(*
  let of_command_line command_line =
    let version_command_line = Printf.sprintf "%s -version" command_line in
    let in_channel = Unix.open_process_in version_command_line in
    let version_line =
      try_close
        ~close:(fun () ->
          assert (in_channel |> Unix.close_process_in = Unix.WEXITED 0))
      @@ fun () -> input_line in_channel in
    of_version_line version_line
*)

  let to_string ?(sep = ".") ?(include_patch = true) { major; minor; patch } =
    let major_minor =
      if major >= 5 then
        Printf.sprintf "%d%s%d" major sep minor
      else
        Printf.sprintf "%d%s%.2d" major sep minor in
    if include_patch then
      Printf.sprintf "%s%s%d" major_minor sep patch
    else
      major_minor
end

let signature_of_in_channel ?filename in_channel =
  let lexbuf = in_channel |> Lexing.from_channel in
  filename |> Option.iter (Lexing.set_filename lexbuf);
  lexbuf |> Parse.interface

(*
module Interpreter = struct
  type t = {
      command_line : string;
      version : Version.t;
    }

  let of_command_line command_line =
    let version = Version.of_command_line command_line in
    { command_line; version }
end
*)

module Buffer = struct
  include Buffer

  let add_channel_no_wait buffer in_channel size =
    let bytes = Bytes.create size in
    let read = input in_channel bytes 0 size in
    Buffer.add_subbytes buffer bytes 0 read;
    read

  let add_channel_to_the_end ?(chunk_size = 4096) ?(continue = fun () -> true)
      buffer in_channel =
    while
      add_channel_no_wait buffer in_channel chunk_size <> 0 && continue () do
      ()
    done

  let suffix_of_length buffer len =
    sub buffer (length buffer - len) len

  let has_suffix buffer suffix =
    length buffer >= String.length suffix &&
    suffix_of_length buffer (String.length suffix) = suffix
end

module String = struct
  include String

  let suffix_of_length s len =
    sub s (length s - len) len

  let has_suffix s ~suffix =
    length s >= length suffix &&
    suffix_of_length s (length suffix) = suffix

  let prefix_of_length s len =
    sub s 0 len

  let has_prefix s ~prefix =
    length s >= length prefix &&
    prefix_of_length s (length prefix) = prefix

  let suffix_from s pos =
    sub s pos (length s - pos)
end
