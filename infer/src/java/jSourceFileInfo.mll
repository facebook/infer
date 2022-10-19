(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{
  open! IStd

  open Lexing

  module Array = struct
    include Array
    let make len = create ~len
  end

  (** classic Ocamllex function to update current lexbuf line at each end of
     line *)
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

  (** position of the char just after lexbuf *)
  let end_pos lexbuf =
    lexbuf.lex_curr_p.pos_lnum,
    lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol

  (** return the exact position start of the suffix [classname] in [lexbuf] *)
  let location_suffix suffix lexbuf =
    let length_suffix = String.length suffix in
    let l, c = end_pos lexbuf in
    l, c - length_suffix

  (** return the position start of [lexbuf] *)
  let location_start lexbuf =
    let l, c = end_pos lexbuf in
    let lexbuf_length = lexeme_end lexbuf - lexeme_start lexbuf in
    l, c - lexbuf_length

  (** We traverse the structure of the source file by recording the
      encompassing blocks in a stack of frame.
      We use the stack to recover the full inner class name at bytecode level *)
  type expr = | AllocExpr | OtherExpr
  type frame =
    { short_class_name: string
    ; is_enum : bool
    ; next_anonymous_class: int
    ; opened_blocks: int
    ; exprs: expr list }
  type state =
    { stack: frame list
    ; record_location: classname:string -> col:int -> line:int -> unit }

  let push frame state = { state with stack = frame :: state.stack; }

  exception Missing_opening_bracket

  exception Missing_opening_parenthesis

  let add_package package state =
    let record_location ~classname =
      let classname = package^"."^classname in
      state.record_location ~classname in
    { state with record_location; }

  let pop_class state =
    match state.stack with
     | [] -> raise Missing_opening_bracket
     | _ :: stack -> { state with stack; }

  let incr_next_anonymous state =
    match state.stack with
    | [] -> { state with stack = []; }
    | fr :: stack ->
        let stack =
         {fr with next_anonymous_class = fr.next_anonymous_class+1; } :: stack in
        { state with stack; }

  let add_expr e (state:state) : state =
    match state.stack with
    | [] -> state
    | fr :: stack ->
        let stack = {fr with exprs = e :: fr.exprs; } :: stack in
        { state with stack; }

  let pop_exprs state =
    match state.stack with
    | [] -> raise Missing_opening_parenthesis
    | fr :: stack -> (
        match fr.exprs with
          | [] -> raise Missing_opening_parenthesis
          | e :: exprs ->
            let stack = {fr with exprs; } :: stack in
            (e, { state with stack; }))

  let in_field_declaration_area state =
    match state.stack with
     | [] -> false
     | fr :: _ -> Int.equal fr.opened_blocks 0

  let get_opened_blocks state =
    match state.stack with
    | [] -> raise Missing_opening_bracket
    | fr :: _ -> fr.opened_blocks

  let is_enum state =
    match state.stack with
    | [] -> false
    | fr :: _ -> fr.is_enum && Int.equal fr.opened_blocks 0

  let get_next_anonymous_class state =
    match state.stack with
    | [] -> raise Missing_opening_bracket
    | fr :: _ -> string_of_int fr.next_anonymous_class

  let decr_opened_blocks state =
    let stack =
      match state.stack with
      | [] -> []
      | fr :: stack ->
          {fr with opened_blocks = fr.opened_blocks-1; } :: stack in
     { state with stack; }

  let incr_opened_blocks state =
    let stack =
      match state.stack with
      | [] -> []
      | fr :: stack ->
          {fr with opened_blocks = fr.opened_blocks+1; } :: stack in
     { state with stack; }

  let long_class_name name state =
    let f name frame = Printf.sprintf "%s$%s" frame.short_class_name name in
    List.fold ~f ~init:name state.stack

}

let whitespace = [' ' '\t']
let eol = whitespace*("\r")?"\n" (* end of line *)
let eol_comment = "//" [^'\n']*
let id = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*
let char = "'\\''" | "'\"'" | "'" [ ^'\'' ]+ "'"
let class_keyword = "class"|"interface"|"enum"|"@" whitespace+ "interface"

(* We follow an abstraction of the official grammar described here:
    https://docs.oracle.com/javase/specs/jls/se14/html/jls-19.html *)
rule class_scan state = parse
  | whitespace+
        { class_scan state lexbuf }
  | eol_comment
        { class_scan state lexbuf }
  | "/*"
        { skip_comments (class_scan state) lexbuf }
  | eol
        { incr_linenum lexbuf;
          class_scan state lexbuf }
  | "package" whitespace+ (id ("." id)* as package) whitespace* ";"
        { class_scan (add_package package state) lexbuf }
  | id
        { class_scan state lexbuf }
  | class_keyword whitespace+ (id as name)
        {
          let line, col = location_suffix name lexbuf in
          let classname = long_class_name name state in
          state.record_location ~classname ~col ~line ;
          let frame : frame =
            { short_class_name = name;
              is_enum = false;
              next_anonymous_class = 1;
              exprs = [];
              opened_blocks = 0 } in
          (* we jump to the next left bracket, skipping annotations and
             generics <...> contents *)
          do_at_next_left_bracket
            (fun lexbuf ->
              class_scan (push frame state) lexbuf) lexbuf
        }
  | "new" whitespace+
        { (* may be a declaration of an anonymous class
             ```new [TypeArguments] ClassOrInterfaceTypeToInstantiate
                    ( [ArgumentList] ) [ClassBody] ```
                                      ^
             so we jump this position |                  *)
          search_anonymous_class_body state lexbuf
        }
  | id
        { if is_enum state
          then found_entrance_of_anonymous_class state lexbuf
          else class_scan state lexbuf }
  | (id as _field) whitespace* ";"
        { if in_field_declaration_area state then
            (* we only reach this situation in class/interface bodies, and
               never inside method bodies *)
             () ; (* TODO : record field location *)
          class_scan state lexbuf
        }
  | "\""
        { skip_string (class_scan state) lexbuf }
  | char
        { class_scan state lexbuf }
  | "{"
        { class_scan (incr_opened_blocks state) lexbuf }
  | "("
        { class_scan (add_expr OtherExpr state) lexbuf }
  | ")"
        { match pop_exprs state with
            | AllocExpr, state -> found_entrance_of_anonymous_class state lexbuf
            | OtherExpr, state -> class_scan state lexbuf }
  | "@" whitespace* id ("." id)* "("
        { skip_well_parenthesized_parentheses 1
            (class_scan state) lexbuf }
  | "}"
        {
          if Int.equal (get_opened_blocks state) 0
          then class_scan (pop_class state) lexbuf
          else class_scan (decr_opened_blocks state) lexbuf
        }
  | _
        { class_scan state lexbuf }
  | eof
        { () }

(* we search for the next left bracket *)
and do_at_next_left_bracket action = parse
  | eol
        { incr_linenum lexbuf;
          do_at_next_left_bracket action lexbuf }
  | eol_comment
        { do_at_next_left_bracket action lexbuf }
  | "/*"
        { skip_comments (do_at_next_left_bracket action) lexbuf }
  | "{"
        { action lexbuf }
  | "<"
        { skip_well_parenthesized_angles 1
             (do_at_next_left_bracket action) lexbuf }
  | "@" whitespace* id "("
        { skip_well_parenthesized_parentheses 1
            (do_at_next_left_bracket action) lexbuf }
  | "\""
        { skip_string (do_at_next_left_bracket action) lexbuf }
  | _
        { do_at_next_left_bracket action lexbuf }

(* we search for (...) parentheses *)
and search_anonymous_class_body state = parse
  | eol
        { incr_linenum lexbuf;
          search_anonymous_class_body state lexbuf }
  | eol_comment
        { search_anonymous_class_body state lexbuf }
  | "/*"
        { skip_comments
            (search_anonymous_class_body state) lexbuf }
  | "("
        { class_scan (add_expr AllocExpr state) lexbuf }
  | "<"
        { skip_well_parenthesized_angles 1
             (search_anonymous_class_body state) lexbuf }
  | "@" whitespace* id "("
        { skip_well_parenthesized_parentheses 1
             (search_anonymous_class_body state) lexbuf }
  | "\""
        { skip_string
             (search_anonymous_class_body state) lexbuf }
  | "["
        { (* this is an array allocation, not an anonymous class *)
          class_scan state lexbuf
        }
  | _
        { search_anonymous_class_body state lexbuf }

(* we test if there is an opening anonymous class body here *)
and found_entrance_of_anonymous_class state = parse
  | eol
        { incr_linenum lexbuf;
          found_entrance_of_anonymous_class state lexbuf }
  | eol_comment
        { found_entrance_of_anonymous_class state lexbuf }
  | "/*"
        { skip_comments
            (found_entrance_of_anonymous_class state) lexbuf }
  | whitespace+
        { found_entrance_of_anonymous_class state lexbuf }
  | "{"
        { (* this is an anonymous class *)
          let line, col = location_start lexbuf in
          let name = get_next_anonymous_class state in
          let classname = long_class_name name state in
          state.record_location ~classname ~col ~line ;
          let frame : frame =
            { short_class_name = name;
              is_enum = false;
              next_anonymous_class = 1;
              exprs = [];
              opened_blocks = 0 } in
          class_scan (push frame (incr_next_anonymous state)) lexbuf
        }
  | _
        { (* this is not an anonymous class *)
          class_scan state lexbuf
        }

(* we skip type arguments <...> because they may contain brackets *)
and skip_well_parenthesized_angles width action = parse
  | eol
        { incr_linenum lexbuf;
           skip_well_parenthesized_angles width action lexbuf }
  | "<"
        { skip_well_parenthesized_angles (width+1) action lexbuf }
  | ">"
        { if width <= 1 then action lexbuf
          else skip_well_parenthesized_angles (width-1) action lexbuf }
  | eol_comment
        { skip_well_parenthesized_angles width action lexbuf }
  | "/*"
        { skip_comments
             (skip_well_parenthesized_angles width action) lexbuf }
  | "\""
        { skip_string (skip_well_parenthesized_angles width action) lexbuf }
  | _
        { skip_well_parenthesized_angles width action lexbuf }

(* we skip type annotation arguments (...) because they may contain brackets *)
and skip_well_parenthesized_parentheses width action = parse
  | eol
        { incr_linenum lexbuf;
           skip_well_parenthesized_parentheses width action lexbuf }
  | "("
        { skip_well_parenthesized_parentheses (width+1) action lexbuf }
  | ")"
        { if width <= 1 then action lexbuf
          else skip_well_parenthesized_parentheses (width-1) action lexbuf }
  | eol_comment
        { skip_well_parenthesized_parentheses width action lexbuf }
  | "/*"
        { skip_comments
             (skip_well_parenthesized_parentheses width action) lexbuf }
  | "\""
        { skip_string (skip_well_parenthesized_parentheses width action) lexbuf }
  | char
        { skip_well_parenthesized_parentheses width action lexbuf }
  | _
        { skip_well_parenthesized_parentheses width action lexbuf }

and skip_string action = parse
  | "\\\\"
        { skip_string action lexbuf }
  | "\\\""
        { skip_string action lexbuf }
  | "\""
        { action lexbuf }
  | _
        { skip_string action lexbuf }

and skip_comments action = parse
  | "*/"
        { action lexbuf }
  | eol
        { incr_linenum lexbuf;
          skip_comments action lexbuf }
  | _
        { skip_comments action lexbuf }

{

  open Javalib_pack

  (** We scan source file [file] and record location of each class declaration *)
  let collect_class_location (program:JProgramDesc.t)(file:SourceFile.t) =
    let path = SourceFile.to_abs_path file in
    if String.is_suffix path ~suffix:".java" then (
      let cin = In_channel.create path in
      let stack = [] in
      let record_location ~classname ~col ~line =
        let loc : Location.t =  { line; col; file; macro_file_opt= None; macro_line= -1 } in
        let cn : JBasics.class_name = JBasics.make_cn classname in
        Logging.debug Capture Verbose "set_java_location %s with location %a@."
          (JBasics.cn_name cn) Location.pp_file_pos loc;
        JProgramDesc.set_java_location program cn loc in
      try (
        class_scan { record_location; stack; } (from_channel cin) ;
        In_channel.close cin )
      with
        | Failure s ->
          Logging.debug Capture Verbose "Error parsing source file %s\n%s"
          (SourceFile.to_abs_path file) s;
          In_channel.close cin
        | Missing_opening_bracket ->
          Logging.debug Capture Verbose
            "Missing opening bracket error while parsing source file %s\n"
            (SourceFile.to_abs_path file);
          In_channel.close cin
        | Missing_opening_parenthesis ->
          Logging.debug Capture Verbose
            "Missing opening parenthesis error while parsing source file %s\n"
            (SourceFile.to_abs_path file);
          In_channel.close cin
    )

  let debug_on_file path =
    if String.is_suffix path ~suffix:".java" then (
      let cin = In_channel.create path in
      let stack = [] in
      let record_location ~classname ~col ~line =
        let cn : JBasics.class_name = JBasics.make_cn classname in
        Printf.printf "class %s at line %d, column %d\n"
          (JBasics.cn_name cn) line col in
      try (
        let state = { record_location; stack; } in
        class_scan state (from_channel cin) ;
        In_channel.close cin )
      with
        | Failure s ->
          Printf.printf "Error parsing source file: %s" s;
          In_channel.close cin
        | Missing_opening_bracket ->
          Printf.printf
            "Missing opening bracket error while parsing source file\n";
          In_channel.close cin
        | Missing_opening_parenthesis ->
          Printf.printf
            "Missing opening parenthesis error while parsing source file\n";
          In_channel.close cin
    )


}
