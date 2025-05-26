let eval_exn str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase false Format.err_formatter phrase

let () =
  assert
    (eval_exn (Printf.sprintf "#directory \"%s\";;" Pymltop_libdir.libdir));
  assert (eval_exn "#install_printer Py.Object.format_repr;;")
