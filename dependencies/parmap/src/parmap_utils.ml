
let log_error fmt =
  Printf.ksprintf (fun s -> Format.eprintf "[Parmap]: %s@.%!" s) fmt

(* tail recursive version of List.append *)
let append_tr l1 l2 =
  let rec aux acc = function
      [] -> acc
    | a::r -> aux (a::acc) r
  in aux l2 (List.rev l1)

(* tail recursive version of List.concat *)
let concat_tr (l: 'a list) =
  List.fold_left (fun acc l -> append_tr l acc) [] (List.rev l)

(* tail recursive version of List.fold_right from ExtLib *)
let fold_right f l init =
  let fold_right_max = 1000 in
  let rec tail_loop acc = function
          | [] -> acc
          | h :: t -> tail_loop (f h acc) t
  in
  let rec loop n = function
          | [] -> init
          | h :: t ->
                  if n < fold_right_max then
                          f h (loop (n+1) t)
                  else
                          f h (tail_loop init (List.rev t))
  in
  loop 0 l

(* would be [? a | a <- startv--endv] using list comprehension from Batteries *)
let range startv endv =
  let s,e = (min startv endv),(max startv endv) in
  let rec aux acc = function n -> if n=s then n::acc else aux (n::acc) (n-1)
  in aux [] e

(* create a shadow file descriptor *)
let tempfd () =
  let name = Filename.temp_file "mmap" "TMP" in
  try
    let fd = Unix.openfile name [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
    Unix.unlink name;
    fd
  with e -> Unix.unlink name; raise e
