(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** Produce output in latex *)

type style = Boldface | Roman | Italics

(** Convert a string to a latex-friendly format *)
let convert_string s =
  if String.contains s '_' then
    let cnt = ref 0 in
    let s' = ref "" in
    let f c =
      if Char.equal c '_' then s' := !s' ^ "\\_" else s' := !s' ^ Char.escaped s.[!cnt] ;
      incr cnt
    in
    String.iter ~f s ; !s'
  else s

(** Print a string in the given style, after converting it into latex-friendly format *)
let pp_string style f s =
  let converted = convert_string s in
  match style with
  | Boldface
   -> F.fprintf f "\\textbf{%s}" converted
  | Roman
   -> F.fprintf f "\\textrm{%s}" converted
  | Italics
   -> F.fprintf f "\\textit{%s}" converted

let color_to_string (c: Pp.color) =
  match c with
  | Black
   -> "black"
  | Blue
   -> "blue"
  | Green
   -> "green"
  | Orange
   -> "orange"
  | Red
   -> "red"

(** Print color command *)
let pp_color f color = F.fprintf f "\\color{%s}" (color_to_string color)

(** Prelude for a latex file with the given author and title *)
let pp_begin f (author, title, table_of_contents) =
  let pp_toc f () = if table_of_contents then F.fprintf f "\\tableofcontents@\n" else () in
  F.fprintf f
    "\\documentclass{article}@\n\\usepackage{hyperref}@\n\\usepackage{color}@\n\\author{%s}@\n\\title{%s}@\n\\begin{document}@\n\\maketitle@\n%a"
    author title pp_toc ()

(** Epilogue for a latex file *)
let pp_end f () = F.fprintf f "\\end{document}@\n"

(** Section with the given title *)
let pp_section f title = F.fprintf f "\\section{%s}@\n" title
