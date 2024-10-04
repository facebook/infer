(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
(* open Core *)

module StringHashtble = String.Table

type tt = (string, string, string) Hashtbl_intf.create_options_without_hashable

let newbindings () = StringHashtble.create ()

(* following monty paper, we hardwire clas instead of having a __class__ attribute in the dictionary - correct?
   note that it's spelled clas because typing the other word makes vscode ocaml crash :-( *)
type pval = {clas: pclass; mval: mval; dict: dictionary}

and pclass = Builtin | Classobj of pval

and mval =
  | MetaNone
  | Cell
  | Int of int
  | String of string
  | Closure of (pval list -> pval)
  | List of pval list

and dictionary = pval StringHashtble.t [@@deriving sexp]

let write_binding v1 key data =
  let d = v1.dict in
  StringHashtble.set d ~key ~data


let get_binding v key =
  let d = v.dict in
  StringHashtble.find d key


exception RuntimeError

let pval_as_list v =
  match v with {mval= List l} -> l (* should check class field too? *) | _ -> raise RuntimeError


exception AttributeNotFound

let rec builtin_get_attr_mro_list objlist name =
  match objlist with
  | [] ->
      raise AttributeNotFound
  | x :: xs -> (
    match get_binding x name with None -> builtin_get_attr_mro_list xs name | Some v -> v )


let builtin_get_attr obj attr_name =
  match get_binding obj attr_name with
  | Some v ->
      v
  | None ->
      let pclass = obj.clas in
      (* direct lookup of clas *)
      let mro_list =
        match pclass with
        | Classobj cv -> (
          match get_binding cv "__mro__" with
          | None ->
              raise AttributeNotFound
          | Some mrov ->
              pval_as_list mrov (* throws if not list *) )
        | Builtin ->
            []
      in
      (* says builtin types don't have attributes, clearly wrong *)
      builtin_get_attr_mro_list mro_list attr_name


let mk_closure qname code =
  let f = {clas= Builtin; mval= Closure code; dict= newbindings ()} in
  write_binding f "__qualname__" qname ;
  f


let mk_int n = {clas= Builtin; mval= Int n; dict= newbindings ()}

let mk_string s = {clas= Builtin; mval= String s; dict= newbindings ()}

let mk_cell v =
  { clas= Builtin
  ; mval= Cell
  ; dict=
      (let d = newbindings () in
       StringHashtble.set d ~key:"contents" ~data:v ;
       d ) }


let mk_undef = {clas= Builtin; mval= MetaNone; dict= newbindings ()}

let store_fast locals v s = Hashtbl.set locals ~key:s ~data:v

(* see https://tenthousandmeters.com/blog/python-behind-the-scenes-5-how-variables-are-implemented-in-cpython/ *)
let store_name = store_fast

let load_name locals globals s =
  match Hashtbl.find locals s with
  | None -> (
    match Hashtbl.find globals s with
    | None ->
        raise RuntimeError (* should go to builtins here *)
    | Some v ->
        v )
  | Some v ->
      v


let store_deref locals v s =
  match Hashtbl.find locals s with
  | None ->
      Hashtbl.set locals ~key:s ~data:(mk_cell v)
  | Some cell_object ->
      write_binding cell_object "contents" v


let load_fast locals s = match Hashtbl.find locals s with None -> raise RuntimeError | Some v -> v

let load_deref locals s =
  match Hashtbl.find locals s with
  | None ->
      raise RuntimeError
  | Some cell_object -> (
    match get_binding cell_object "contents" with None -> raise RuntimeError | Some v -> v )


let load_closure = load_fast

let load_global globals s =
  match Hashtbl.find globals s with
  | None ->
      raise RuntimeError (* should go to builtins *)
  | Some v ->
      v


(* this seems questionable - docs say "checks locals before the cell" but I'm putting the cell *in* the locals
   so not convinced this is right *)
let load_classderef locals s =
  match Hashtbl.find locals s with
  | None ->
      raise RuntimeError
  | Some ({mval= Cell} as cell_object) -> (
    match get_binding cell_object "contents" with None -> raise RuntimeError | Some v -> v )
  | Some v ->
      v


let store_global globals v s = Hashtbl.set globals ~key:s ~data:v

(* MAKE_FUNCTION (simplified version)
   returns a function object
   takes as args a qualified name, a code object (which will be an OCaml function expecting locals and globals) and
   in the real bytecode a tuple of values, but in our version is the initial locals map of string names to values
   when we call MAKE_FUNCTION we copy the current globals into the function object's globals field
   Oh, we can do that with partial application if the Ocaml function takes globals first
   (and we don't write globals itself, or access __globals__ (which we actually do in some bits of fbcode))
   Oh, no, let's just have the Ocaml code extend locals in its preamble, so it always takes a list of values, corresponding
   let mypythonfun globalsmap localvaluelist arglist
     let locals = makelocalbindings ["x", "y", "z"] (localvaluelist ++ arglist) where makelocalbindings creates a new hashtable
     in
   and we do make_function localvaluelist mypythonfunction name = mk_closure name (mypythonfunction globals localvaluelist)
*)

let make_function globals localvaluelist mypythonfunction name =
  mk_closure name (mypythonfunction globals localvaluelist)


let makelocalbindings namelist valuelist =
  let alist = List.zip_exn namelist valuelist in
  Hashtbl.of_alist_exn (module String) alist


let call_function funcobj arglist =
  match funcobj with {mval= Closure code} -> code arglist | _ -> raise RuntimeError


(* use in function preamble to create cell variables that are accessed before being written *)
let add_cell_variables locals varnames =
  List.iter varnames ~f:(fun v -> Hashtbl.set locals ~key:v ~data:(mk_cell mk_undef))


let python_print_function =
  mk_closure (mk_string "print") (fun args ->
      match args with
      | [x] ->
          sexp_of_pval x |> Sexplib.Sexp.to_string |> printf "%s\n" ;
          mk_int 0
      | _ ->
          raise RuntimeError )


(*let python_build_class_function =
  mk_closure (mk_string "build_class") (fun args ->
    match args with
    | class_initialiser::class_name::supers ->

    | _ -> raise RuntimeError)
*)

(* EXAMPLES *)

(*
def f(x):
        return x

    print (f(3))

  2           0 LOAD_CONST               1 (<code object f at 0x7f4c61a1dd90, file "/tmp/ipykernel_732192/2963205528.py", line 2>)
              2 LOAD_CONST               2 ('wrapper.<locals>.f')
              4 MAKE_FUNCTION            0
              6 STORE_FAST               0 (f)

  5           8 LOAD_GLOBAL              0 (print)
             10 LOAD_FAST                0 (f)
             12 LOAD_CONST               3 (3)
             14 CALL_FUNCTION            1
             16 CALL_FUNCTION            1
             18 POP_TOP
             20 LOAD_CONST               0 (None)
             22 RETURN_VALUE

Disassembly of <code object f at 0x7f4c61a1dd90, file "/tmp/ipykernel_732192/2963205528.py", line 2>:
  3           0 LOAD_FAST                0 (x)
              2 RETURN_VALUE

*)

let f_code _globals localvalues arglist =
  let locals = makelocalbindings ["x"] (localvalues @ arglist) in
  load_fast locals "x"


let wrapper_code globals localvalues arglist =
  let locals = makelocalbindings [] (localvalues @ arglist) in
  let f = make_function globals [] f_code (mk_string "wrapper.<locals>.f") in
  let _ = store_fast locals f "f" in
  let f = load_fast locals "f" in
  let res = call_function f [mk_int 3] in
  let print = load_global globals "print" in
  let _ = call_function print [res] in
  mk_int 0


(*
  def f():
        def g():
            print(a)
        a = 'assigned'
        g()
        a = 'reassigned'
        g()

  f()

  2           0 LOAD_CONST               1 (<code object f at 0x7f4c61a1f690, file "/tmp/ipykernel_732192/2607903886.py", line 2>)
              2 LOAD_CONST               2 ('wrapper2.<locals>.f')
              4 MAKE_FUNCTION            0
              6 STORE_FAST               0 (f)

 10           8 LOAD_FAST                0 (f)
             10 CALL_FUNCTION            0
             12 POP_TOP
             14 LOAD_CONST               0 (None)
             16 RETURN_VALUE

Disassembly of <code object f at 0x7f4c61a1f690, file "/tmp/ipykernel_732192/2607903886.py", line 2>:
  3           0 LOAD_CLOSURE             0 (a)
              2 BUILD_TUPLE              1
              4 LOAD_CONST               1 (<code object g at 0x7f4c61a1f550, file "/tmp/ipykernel_732192/2607903886.py", line 3>)
              6 LOAD_CONST               2 ('wrapper2.<locals>.f.<locals>.g')
              8 MAKE_FUNCTION            8 (closure)
             10 STORE_FAST               0 (g)

  5          12 LOAD_CONST               3 ('assigned')
             14 STORE_DEREF              0 (a)

  6          16 LOAD_FAST                0 (g)
             18 CALL_FUNCTION            0
             20 POP_TOP

  7          22 LOAD_CONST               4 ('reassigned')
             24 STORE_DEREF              0 (a)

  8          26 LOAD_FAST                0 (g)
             28 CALL_FUNCTION            0
             30 POP_TOP
             32 LOAD_CONST               0 (None)
             34 RETURN_VALUE

Disassembly of <code object g at 0x7f4c61a1f550, file "/tmp/ipykernel_732192/2607903886.py", line 3>:
  4           0 LOAD_GLOBAL              0 (print)
              2 LOAD_DEREF               0 (a)
              4 CALL_FUNCTION            1
              6 POP_TOP
              8 LOAD_CONST               0 (None)
             10 RETURN_VALUE
*)

let g_code globals localvalues arglist =
  let locals = makelocalbindings ["a"] (localvalues @ arglist) in
  let print = load_global globals "print" in
  let a = load_deref locals "a" in
  let _ = call_function print [a] in
  mk_int 0


let f_code globals localvalues arglist =
  let locals = makelocalbindings [] (localvalues @ arglist) in
  (* note that cell variables aren't reflected directly in the instruction stream but in the co_cellvars of the code object
     we could first-class code objects a little more, but this seems the right level for textual translation
  *)
  let _ = add_cell_variables locals ["a"] in
  let a = load_closure locals "a" in
  let g = make_function globals [a] g_code (mk_string "wrapper2.<locals>.f.<locals>.g") in
  let _ = store_fast locals g "g" in
  let _ = store_deref locals (mk_string "assigned") "a" in
  let g = load_fast locals "g" in
  let _ = call_function g [] in
  let _ = store_deref locals (mk_string "reassigned") "a" in
  let g = load_fast locals "g" in
  let _ = call_function g [] in
  mk_int 0


let wrapper2_code globals localvalues arglist =
  let locals = makelocalbindings [] (localvalues @ arglist) in
  let f = make_function globals [] f_code (mk_string "wrapper2.<locals>.f") in
  let _ = store_fast locals f "f" in
  let f = load_fast locals "f" in
  let _ = call_function f [] in
  mk_int 0


(*
def f():
    def g():
        nonlocal a
        a = 'reassigned'
    a = 'assigned'
    print(a)
    g()
    print(a)

f()

  2           0 LOAD_CONST               1 (<code object f at 0x7f4c5c978c10, file "/tmp/ipykernel_732192/3368925528.py", line 2>)
              2 LOAD_CONST               2 ('wrapper3.<locals>.f')
              4 MAKE_FUNCTION            0
              6 STORE_FAST               0 (f)

 11           8 LOAD_FAST                0 (f)
             10 CALL_FUNCTION            0
             12 POP_TOP
             14 LOAD_CONST               0 (None)
             16 RETURN_VALUE

Disassembly of <code object f at 0x7f4c5c978c10, file "/tmp/ipykernel_732192/3368925528.py", line 2>:
  3           0 LOAD_CLOSURE             0 (a)
              2 BUILD_TUPLE              1
              4 LOAD_CONST               1 (<code object g at 0x7f4c61afbcd0, file "/tmp/ipykernel_732192/3368925528.py", line 3>)
              6 LOAD_CONST               2 ('wrapper3.<locals>.f.<locals>.g')
              8 MAKE_FUNCTION            8 (closure)
             10 STORE_FAST               0 (g)

  6          12 LOAD_CONST               3 ('assigned')
             14 STORE_DEREF              0 (a)

  7          16 LOAD_GLOBAL              0 (print)
             18 LOAD_DEREF               0 (a)
             20 CALL_FUNCTION            1
             22 POP_TOP

  8          24 LOAD_FAST                0 (g)
             26 CALL_FUNCTION            0
             28 POP_TOP

  9          30 LOAD_GLOBAL              0 (print)
             32 LOAD_DEREF               0 (a)
             34 CALL_FUNCTION            1
             36 POP_TOP
             38 LOAD_CONST               0 (None)
             40 RETURN_VALUE

Disassembly of <code object g at 0x7f4c61afbcd0, file "/tmp/ipykernel_732192/3368925528.py", line 3>:
  5           0 LOAD_CONST               1 ('reassigned')
              2 STORE_DEREF              0 (a)
              4 LOAD_CONST               0 (None)
              6 RETURN_VALUE
*)

let g_code _globals localvalues arglist =
  let locals = makelocalbindings ["a"] (localvalues @ arglist) in
  let _ = store_deref locals (mk_string "reassigned") "a" in
  mk_int 0


let f_code globals localvalues arglist =
  let locals = makelocalbindings [] (localvalues @ arglist) in
  let _ = add_cell_variables locals ["a"] in
  let a = load_closure locals "a" in
  let g = make_function globals [a] g_code (mk_string "wrapper3.<locals>.f.<locals>.g") in
  let _ = store_fast locals g "g" in
  let _ = store_deref locals (mk_string "assigned") "a" in
  let print = load_global globals "print" in
  let a = load_deref locals "a" in
  let _ = call_function print [a] in
  let g = load_fast locals "g" in
  let _ = call_function g [] in
  let print = load_global globals "print" in
  let a = load_deref locals "a" in
  let _ = call_function print [a] in
  mk_int 0


let wrapper3_code globals localvalues arglist =
  let locals = makelocalbindings [] (localvalues @ arglist) in
  let f = make_function globals [] f_code (mk_string "wrapper3.<locals>.f") in
  let _ = store_fast locals f "f" in
  let f = load_fast locals "f" in
  let _ = call_function f [] in
  mk_int 0


(* default outer globals map - or should this be in builtins? *)
let globals =
  let bindings = newbindings () in
  Hashtbl.set bindings ~key:"print" ~data:python_print_function ;
  bindings


(* test some code with no args or free variables and print the result *)
let test wrapper =
  let res = wrapper globals [] [] in
  sexp_of_pval res |> Sexplib.Sexp.to_string |> printf "%s\n"


let () =
  printf "pysem\n" ;
  test wrapper_code ;
  printf "wrapper2\n" ;
  test wrapper2_code ;
  printf "wrapper3\n" ;
  test wrapper3_code
