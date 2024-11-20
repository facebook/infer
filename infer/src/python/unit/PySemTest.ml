(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module StringHashtble = Stdlib.Hashtbl.Make (String)

let newbindings () = StringHashtble.create 17

(* following monty paper, we hardwire clas instead of having a __class__ attribute in the dictionary - correct?
   note that it's spelled clas because typing the other word makes vscode ocaml crash :-( *)
type pval = {clas: pclass; mval: mval; dict: dictionary}

and pclass = Builtin | Classobj of pval

and mval =
  | MetaNone
  | Cell
  | Int of int
  | String of string
  | Closure of locals * (pval list -> pval)
  | List of pval list

and dictionary = pval StringHashtble.t

and locals = {fastlocals: dictionary; locals: dictionary}

let rec pp_pval fmt {clas; mval; dict} =
  F.fprintf fmt "[class=%a; mval=%a; dict=%a]" pp_pclass clas pp_mval mval pp_dictionnary dict


and pp_mval fmt = function
  | MetaNone ->
      F.pp_print_string fmt "None"
  | Cell ->
      F.pp_print_string fmt "Cell"
  | Int i ->
      F.pp_print_int fmt i
  | String s ->
      Format.pp_print_string fmt s
  | Closure _ ->
      F.pp_print_string fmt "Closure"
  | List l ->
      F.fprintf fmt "[%a]" (Pp.comma_seq pp_pval) l


and pp_pclass fmt = function
  | Builtin ->
      F.pp_print_string fmt "Builtin"
  | Classobj v ->
      pp_pval fmt v


and pp_dictionnary fmt dict =
  StringHashtble.to_seq dict |> ListLabels.of_seq
  |> PrettyPrintable.pp_collection fmt ~pp_item:(fun fmt (key, v) ->
         F.fprintf fmt "%s: %a" key pp_pval v )


type code_object =
  { co_freevars: string list
  ; co_varnames: string list
  ; co_cellvars: string list
  ; co_names: string list
  ; co_argcount: int
  ; code: locals -> pval }

let write_binding v1 key data =
  let d = v1.dict in
  StringHashtble.replace d key data


let get_binding v key =
  let d = v.dict in
  StringHashtble.find_opt d key


exception RuntimeError

let pval_as_list v =
  match v with {mval= List l} -> l (* should check class field too? *) | _ -> raise RuntimeError


exception AttributeNotFound of string

exception LocalNotFound of string

let rec builtin_get_attr_mro_list objlist name =
  match objlist with
  | [] ->
      raise (AttributeNotFound name)
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
              raise (AttributeNotFound "__mro__")
          | Some mrov ->
              pval_as_list mrov (* throws if not list *) )
        | Builtin ->
            []
        (* says builtin types don't have attributes, clearly wrong and needs fixing *)
      in
      builtin_get_attr_mro_list mro_list attr_name


let makelocalbindings namelist valuelist =
  let alist = List.zip_exn namelist valuelist in
  ListLabels.to_seq alist |> StringHashtble.of_seq


let mk_int n = {clas= Builtin; mval= Int n; dict= newbindings ()}

let mk_string s = {clas= Builtin; mval= String s; dict= newbindings ()}

let mk_list l = {clas= Builtin; mval= List l; dict= newbindings ()}

let mk_undef = {clas= Builtin; mval= MetaNone; dict= newbindings ()}

let mk_cell v =
  { clas= Builtin
  ; mval= Cell
  ; dict=
      (let d = newbindings () in
       StringHashtble.replace d "contents" v ;
       d ) }


let extendslocalbindings locals cellvarlist namelist valuelist =
  let alist = List.zip_exn namelist valuelist in
  List.iter alist ~f:(fun (key, rawdata) ->
      let data =
        if List.mem cellvarlist key ~equal:String.equal then mk_cell rawdata else rawdata
      in
      StringHashtble.replace locals key data )


let mk_closure qname code_obj local_values =
  (* I *think* free vars are never in cellvars so this is the right thing to do *)
  let fastlocals = makelocalbindings code_obj.co_freevars local_values in
  let locals = {fastlocals; locals= newbindings ()} in
  let f =
    { clas= Builtin
    ; mval=
        Closure
          ( locals
          , fun argvals ->
              (* now the first co_argcount members of co_varnames are the function arguments
                  When we bind them, we check if they're in co_cellvars - if not we bind the
                 name to the value, else we bind the name to a cell containing the value
                  The remainder are local names that we initialise with None or Cell(None)
                  as appropriate *)
              let argument_names, _local_names =
                List.split_n code_obj.co_varnames code_obj.co_argcount
              in
              extendslocalbindings fastlocals code_obj.co_cellvars argument_names argvals ;
              (* List.iter local_names ~f:(fun name ->
                  if List.mem code_obj.co_cellvars name ~equal:String.equal then
                    Hashtbl.set dict ~key:name ~data:(mk_cell mk_undef) ) ;*)
              List.iter code_obj.co_cellvars ~f:(fun name ->
                  if List.mem argument_names name ~equal:String.equal then ()
                    (* already initialised *)
                  else StringHashtble.replace fastlocals name (mk_cell mk_undef) ) ;
              code_obj.code locals )
    ; dict= locals.locals }
  in
  write_binding f "__qualname__" qname ;
  f


let store_fast {fastlocals} v s = StringHashtble.replace fastlocals s v

(* see https://tenthousandmeters.com/blog/python-behind-the-scenes-5-how-variables-are-implemented-in-cpython/ *)
let store_name {locals} v s = StringHashtble.replace locals s v

let load_name {locals} globals s =
  match StringHashtble.find_opt locals s with
  | None -> (
    match StringHashtble.find_opt globals s with
    | None ->
        raise RuntimeError (* should go to builtins here *)
    | Some v ->
        v )
  | Some v ->
      v


let store_deref {fastlocals} v s =
  match StringHashtble.find_opt fastlocals s with
  | None ->
      StringHashtble.replace fastlocals s (mk_cell v)
      (* reading the source, I think this shouldn't happen *)
  | Some cell_object ->
      write_binding cell_object "contents" v


let load_fast {fastlocals} s =
  match StringHashtble.find_opt fastlocals s with None -> raise (LocalNotFound s) | Some v -> v


let load_deref {fastlocals} s =
  match StringHashtble.find_opt fastlocals s with
  | None ->
      raise (LocalNotFound s)
  | Some cell_object -> (
    match get_binding cell_object "contents" with
    | None ->
        F.printf "got binding that should be a cell, it's %a" pp_pval cell_object ;
        raise (AttributeNotFound "cell contents")
    | Some v ->
        v )


let load_closure = load_fast

let load_global globals s =
  match StringHashtble.find_opt globals s with
  | None ->
      raise RuntimeError (* should go to builtins *)
  | Some v ->
      v


let load_classderef {fastlocals; locals} s =
  match StringHashtble.find_opt locals s with
  | Some v ->
      v
  | None -> (
    match StringHashtble.find_opt fastlocals s with
    | Some ({mval= Cell} as cell_object) -> (
      match get_binding cell_object "contents" with None -> raise RuntimeError | Some v -> v )
    | Some _v ->
        printf "found non-cell for %s in classderef" s ;
        raise RuntimeError
    | _ ->
        printf "failed to find binding in load classderef on %s\n" s ;
        raise RuntimeError )


let store_global globals v s = StringHashtble.replace globals s v

(* MAKE_FUNCTION (simplified version)
*)

let make_function globals localvaluelist mypythonfunction name =
  mk_closure name (mypythonfunction globals) localvaluelist


let call_function funcobj arglist =
  match funcobj with {mval= Closure (_, code)} -> code arglist | _ -> raise RuntimeError


let get_locals_of_closure closure =
  match closure with {mval= Closure (locals, _code)} -> locals | _ -> raise RuntimeError


(* use in function preamble to create cell variables that are accessed before being written
      should now be redundant as added to the calling convention and co_cellvars list in code objects

   let add_cell_variables locals varnames =
     List.iter varnames ~f:(fun v -> StringHashtble.set locals ~key:v ~data:(mk_cell mk_undef))
*)

let python_print_function =
  mk_closure (mk_string "print")
    { co_freevars= []
    ; co_varnames= ["x"]
    ; co_cellvars= []
    ; co_argcount= 1
    ; co_names= []
    ; code=
        (fun argdict ->
          let x_val = load_fast argdict "x" in
          F.printf "%a@\n" pp_pval x_val ;
          mk_int 0 ) }
    []


(*
to build a class, we get a closure and a name
we run the closure, which may have side effects etc, and also use do various store_names to write e.g. method definitions
we want to return a new class object, which is itself callable to create new instances (including calling the __init__ method)
*)

(* we don't make a closure out of this so that it can be called with a normal call_function
   instead the translation of the codestream LOAD_BUILD_CLASS ... CALL_FUNCTION n is just ... build_class theclosure name supers
   note that the mro *)
let build_class closure name supers =
  let _ = call_function closure [] in
  let locals = get_locals_of_closure closure in
  (* TODO use supers to correctly calculate mro, call __init__ on the newly created object *)
  (* also note fancy recursive value *)
  let rec theclass =
    { clas= Builtin
    ; mval=
        Closure
          (locals, fun _argvals -> {clas= Classobj theclass; mval= MetaNone; dict= newbindings ()})
    ; dict= locals.locals }
  in
  let mro = mk_list (theclass :: supers) in
  write_binding theclass "__name__" name ;
  write_binding theclass "__mro__" mro ;
  theclass


let load_method obj mname =
  let m = builtin_get_attr obj mname in
  (* this is a bound method, not sure if it should be first-classed as a pval, but for now let's not bother
     TODO: there should also be a case with null in instead of the object if we've got a function not a method *)
  (m, obj)


let call_method (m, obj) arglist = call_function m (obj :: arglist)

(* *)

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

let f_code _globals =
  { co_freevars= []
  ; co_varnames= ["x"]
  ; co_argcount= 1
  ; co_cellvars= []
  ; co_names= ["print"]
  ; code= (fun locals -> load_fast locals "x") }


let wrapper_code globals =
  { co_freevars= []
  ; co_varnames= ["f"]
  ; co_argcount= 0
  ; co_cellvars= []
  ; co_names= ["print"]
  ; code=
      (fun locals ->
        let f = make_function globals [] f_code (mk_string "wrapper.<locals>.f") in
        let _ = store_fast locals f "f" in
        let f = load_fast locals "f" in
        let res = call_function f [mk_int 3] in
        let print = load_global globals "print" in
        let _ = call_function print [res] in
        mk_int 0 ) }


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

let g_code globals =
  { co_freevars= ["a"]
  ; co_varnames= []
  ; co_cellvars= []
  ; co_argcount= 0
  ; co_names= ["print"]
  ; code=
      (fun locals ->
        let print = load_global globals "print" in
        let a = load_deref locals "a" in
        let _ = call_function print [a] in
        mk_int 0 ) }


let f_code globals =
  { co_freevars= []
  ; co_varnames= ["g"]
  ; co_cellvars= ["a"]
  ; co_argcount= 0
  ; co_names= []
  ; code=
      (fun locals ->
        let a = load_closure locals "a" in
        let g = make_function globals [a] g_code (mk_string "wrapper2.<locals>.f.<locals>.g") in
        let _ = store_fast locals g "g" in
        let _ = store_deref locals (mk_string "assigned") "a" in
        let g = load_fast locals "g" in
        let _ = call_function g [] in
        let _ = store_deref locals (mk_string "reassigned") "a" in
        let g = load_fast locals "g" in
        let _ = call_function g [] in
        mk_int 0 ) }


let wrapper2_code globals =
  { co_freevars= []
  ; co_varnames= ["f"]
  ; co_cellvars= []
  ; co_argcount= 0
  ; co_names= []
  ; code=
      (fun locals ->
        let f = make_function globals [] f_code (mk_string "wrapper2.<locals>.f") in
        let _ = store_fast locals f "f" in
        let f = load_fast locals "f" in
        let _ = call_function f [] in
        mk_int 0 ) }


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

  2           0 LOAD_CONST               1 (<code object f at 0x7fa6a011c170, file "/tmp/ipykernel_238840/1738692392.py", line 2>)
              2 LOAD_CONST               2 ('wrapper3.<locals>.f')
              4 MAKE_FUNCTION            0
              6 STORE_FAST               0 (f)

 11           8 LOAD_GLOBAL              0 (dump_clos)
             10 LOAD_FAST                0 (f)
             12 CALL_FUNCTION            1
             14 POP_TOP

 12          16 LOAD_FAST                0 (f)
             18 CALL_FUNCTION            0
             20 POP_TOP
             22 LOAD_CONST               0 (None)
             24 RETURN_VALUE

Disassembly of <code object f at 0x7fa6a011c170, file "/tmp/ipykernel_238840/1738692392.py", line 2>:
  3           0 LOAD_CLOSURE             0 (a)
              2 BUILD_TUPLE              1
              4 LOAD_CONST               1 (<code object g at 0x7fa6a44e35f0, file "/tmp/ipykernel_238840/1738692392.py", line 3>)
              6 LOAD_CONST               2 ('wrapper3.<locals>.f.<locals>.g')
              8 MAKE_FUNCTION            8 (closure)
             10 STORE_FAST               0 (g)

  6          12 LOAD_GLOBAL              0 (dump_clos)
             14 LOAD_FAST                0 (g)
             16 CALL_FUNCTION            1
             18 POP_TOP

  7          20 LOAD_CONST               3 ('assigned')
             22 STORE_DEREF              0 (a)

  8          24 LOAD_GLOBAL              1 (print)
             26 LOAD_DEREF               0 (a)
             28 CALL_FUNCTION            1
             30 POP_TOP

  9          32 LOAD_FAST                0 (g)
             34 CALL_FUNCTION            0
             36 POP_TOP

 10          38 LOAD_GLOBAL              1 (print)
             40 LOAD_DEREF               0 (a)
             42 CALL_FUNCTION            1
             44 POP_TOP
             46 LOAD_CONST               0 (None)
             48 RETURN_VALUE

Disassembly of <code object g at 0x7fa6a44e35f0, file "/tmp/ipykernel_238840/1738692392.py", line 3>:
  5           0 LOAD_CONST               1 ('reassigned')
              2 STORE_DEREF              0 (a)
              4 LOAD_CONST               0 (None)
              6 RETURN_VALUE
*)

let g_code _globals =
  { co_freevars= ["a"]
  ; co_varnames= []
  ; (* note that cellvars don't include the freevars, even if they will be accessed with deref *)
    co_cellvars= []
  ; co_argcount= 0
  ; co_names= []
  ; code=
      (fun locals ->
        let _ = store_deref locals (mk_string "reassigned") "a" in
        (* store_deref 0 with freevars[0] = "a" *)
        mk_int 0 ) }


let f_code globals =
  { co_freevars= []
  ; co_varnames= ["g"]
  ; co_cellvars= ["a"]
  ; co_argcount= 0
  ; co_names= ["print"]
  ; code=
      (fun locals ->
        let a = load_closure locals "a" in
        (* load_closure 0 with co_cellvars[0] = "a" *)
        let g = make_function globals [a] g_code (mk_string "wrapper3.<locals>.f.<locals>.g") in
        let _ = store_fast locals g "g" in
        (* store_fast 0 with co_varnames[0]="g" *)
        let _ = store_deref locals (mk_string "assigned") "a" in
        (* store_deref 0 with cellvars[0]="a" *)
        let print = load_global globals "print" in
        let a = load_deref locals "a" in
        (* load_deref 0, cellvars *)
        let _ = call_function print [a] in
        let g = load_fast locals "g" in
        (* load_fast 0, varnames *)
        let _ = call_function g [] in
        let print = load_global globals "print" in
        let a = load_deref locals "a" in
        let _ = call_function print [a] in
        mk_int 0 ) }


let wrapper3_code globals =
  { co_freevars= []
  ; co_varnames= ["f"]
  ; co_argcount= 0
  ; co_cellvars= []
  ; co_names= []
  ; code=
      (fun locals ->
        let f = make_function globals [] f_code (mk_string "wrapper3.<locals>.f") in
        let _ = store_fast locals f "f" in
        let f = load_fast locals "f" in
        let _ = call_function f [] in
        mk_int 0 ) }


(* class C:
           def get(self):
               return 3
       print(C().get())

     2           0 LOAD_BUILD_CLASS
                 2 LOAD_CONST               1 (<code object C at 0x7f3be84270f0, file "/tmp/ipykernel_195093/1918532600.py", line 2>)
                 4 LOAD_CONST               2 ('C')
                 6 MAKE_FUNCTION            0
                 8 LOAD_CONST               2 ('C')
                10 CALL_FUNCTION            2
                12 STORE_FAST               0 (C)

     5          14 LOAD_GLOBAL              0 (print)
                16 LOAD_FAST                0 (C)
                18 CALL_FUNCTION            0
                20 LOAD_METHOD              1 (get)
                22 CALL_METHOD              0
                24 CALL_FUNCTION            1
                26 POP_TOP
                28 LOAD_CONST               0 (None)
                30 RETURN_VALUE

   Disassembly of <code object C at 0x7f3be84270f0, file "/tmp/ipykernel_195093/1918532600.py", line 2>:
     2           0 LOAD_NAME                0 (__name__)
                 2 STORE_NAME               1 (__module__)
                 4 LOAD_CONST               0 ('wrapper5.<locals>.C')
                 6 STORE_NAME               2 (__qualname__)

     3           8 LOAD_CONST               1 (<code object get at 0x7f3be8426fb0, file "/tmp/ipykernel_195093/1918532600.py", line 3>)
                10 LOAD_CONST               2 ('wrapper5.<locals>.C.get')
                12 MAKE_FUNCTION            0
                14 STORE_NAME               3 (get)
                16 LOAD_CONST               3 (None)
                18 RETURN_VALUE

   Disassembly of <code object get at 0x7f3be8426fb0, file "/tmp/ipykernel_195093/1918532600.py", line 3>:
     4           0 LOAD_CONST               1 (3)
                 2 RETURN_VALUE
*)

let get_code _globals =
  { co_freevars= []
  ; co_varnames= ["self"]
  ; co_argcount= 1
  ; co_cellvars= []
  ; co_names= []
  ; code=
      (fun _locals ->
        let t = mk_int 3 in
        t ) }


let c_code globals =
  { co_freevars= []
  ; co_varnames= []
  ; co_argcount= 0
  ; co_cellvars= []
  ; co_names= ["__name__"; "__module__"; "__qualname__"; "get"]
  ; code=
      (fun locals ->
        (* skipping __name__ and __module__ for the moment, 'cos I don't really know where they come from *)
        let c = mk_string "wrapper5.<locals>.C" in
        let _ = store_name locals c "__qualname__" in
        let get_name = mk_string "wrapper5.<locals>.C.get" in
        let get = make_function globals [] get_code get_name in
        let _ = store_name locals get "get" in
        (* this should be None, not zero *)
        mk_int 0 ) }


let wrapper5_code globals =
  { co_freevars= []
  ; co_varnames= ["C"]
  ; co_argcount= 0
  ; co_cellvars= []
  ; co_names= ["print"; "get"]
  ; code=
      (fun locals ->
        let c_clos = make_function globals [] c_code (mk_string "C") in
        let c_clas = build_class c_clos (mk_string "C") [] in
        let _ = store_fast locals c_clas "C" in
        let c_clas = load_fast locals "C" in
        let o = call_function c_clas [] in
        let bm = load_method o "get" in
        let r = call_method bm [] in
        let print = load_global globals "print" in
        let _ = call_function print [r] in
        mk_int 0 ) }


(*
from the full monty paper

def wrapper0():
    def f(x,y):
        print(x); print(y);print("")
        class c:
            x = 4
            print(x); print(y)
            print("")
            def g(self):
                print(x);print(y);print('')
        return c

    f(0,1)().g()

  3           0 LOAD_CONST               1 (<code object f at 0x7f3be8427d70, file "/tmp/ipykernel_195093/4173743622.py", line 3>)
              2 LOAD_CONST               2 ('wrapper0.<locals>.f')
              4 MAKE_FUNCTION            0
              6 STORE_FAST               0 (f)

 13           8 LOAD_FAST                0 (f)
             10 LOAD_CONST               3 (0)
             12 LOAD_CONST               4 (1)
             14 CALL_FUNCTION            2
             16 CALL_FUNCTION            0
             18 LOAD_METHOD              0 (g)
             20 CALL_METHOD              0
             22 POP_TOP
             24 LOAD_CONST               0 (None)
             26 RETURN_VALUE

Disassembly of <code object f at 0x7f3be8427d70, file "/tmp/ipykernel_195093/4173743622.py", line 3>:
  4           0 LOAD_GLOBAL              0 (print)
              2 LOAD_DEREF               1 (x)
              4 CALL_FUNCTION            1
              6 POP_TOP
              8 LOAD_GLOBAL              0 (print)
             10 LOAD_DEREF               2 (y)
             12 CALL_FUNCTION            1
             14 POP_TOP
             16 LOAD_GLOBAL              0 (print)
             18 LOAD_CONST               1 ('')
             20 CALL_FUNCTION            1
             22 POP_TOP

  5          24 LOAD_BUILD_CLASS

             28 LOAD_CLOSURE             1 (x)
             30 LOAD_CLOSURE             2 (y)
             32 BUILD_TUPLE              2
             34 LOAD_CONST               2 (<code object c at 0x7f3be8427af0, file "/tmp/ipykernel_195093/4173743622.py", line 5>)
             36 LOAD_CONST               3 ('c')
             38 MAKE_FUNCTION            8 (closure)
             40 LOAD_CONST               3 ('c')
             42 CALL_FUNCTION            2
             44 STORE_DEREF              0 (c)

 11          46 LOAD_DEREF               0 (c)
             48 RETURN_VALUE

Disassembly of <code object c at 0x7f3be8427af0, file "/tmp/ipykernel_195093/4173743622.py", line 5>:
  5           0 LOAD_NAME                0 (__name__)
              2 STORE_NAME               1 (__module__)
              4 LOAD_CONST               0 ('wrapper0.<locals>.f.<locals>.c')
              6 STORE_NAME               2 (__qualname__)

  6           8 LOAD_CONST               1 (4)
             10 STORE_NAME               3 (x)

  7          12 LOAD_NAME                4 (print)
             14 LOAD_NAME                3 (x)
             16 CALL_FUNCTION            1
             18 POP_TOP
             20 LOAD_NAME                4 (print)
             22 LOAD_CLASSDEREF          2 (y)
             24 CALL_FUNCTION            1
             26 POP_TOP

  8          28 LOAD_NAME                4 (print)
             30 LOAD_CONST               2 ('')
             32 CALL_FUNCTION            1
             34 POP_TOP

             38 LOAD_CLOSURE             1 (x)
             40 LOAD_CLOSURE             2 (y)
             42 BUILD_TUPLE              2
             44 LOAD_CONST               3 (<code object g at 0x7f3be8425610, file "/tmp/ipykernel_195093/4173743622.py", line 9>)
             46 LOAD_CONST               4 ('wrapper0.<locals>.f.<locals>.c.g')
             48 MAKE_FUNCTION            8 (closure)
             50 STORE_NAME               5 (g)
             52 LOAD_CONST               5 (None)
             54 RETURN_VALUE

Disassembly of <code object g at 0x7f3be8425610, file "/tmp/ipykernel_195093/4173743622.py", line 9>:
 10           0 LOAD_GLOBAL              0 (print)
              2 LOAD_DEREF               1 (x)
              4 CALL_FUNCTION            1
              6 POP_TOP
              8 LOAD_GLOBAL              0 (print)
             10 LOAD_DEREF               2 (y)
             12 CALL_FUNCTION            1
             14 POP_TOP
             24 LOAD_CONST               0 (None)
             26 RETURN_VALUE
*)

let g_code globals =
  { co_freevars= ["x"; "y"]
  ; co_varnames= ["self"]
  ; co_argcount= 1
  ; (* includes self *)
    co_cellvars= []
  ; co_names= ["print"]
  ; code=
      (fun locals ->
        let print = load_global globals "print" in
        (* really load global 0, with co_names[0] = "print" *)
        let x = load_deref locals "x" in
        (* load_deref 0, with co_freevars[0] = "x" *)
        let _ = call_function print [x] in
        let y = load_deref locals "y" in
        (* load deref 1, with co_freevars[1] = "y" *)
        let _ = call_function print [y] in
        (* skipping printing c as it wasn't in the original test and I'm not yet sure it'll do the right thing *)
        mk_int 0 ) }


let c_code globals =
  { co_freevars= ["x"; "y"]
  ; co_varnames= []
  ; co_argcount= 0
  ; co_cellvars= []
  ; co_names= ["__name__"; "__module__"; "__qualname__"; "x"; "print"; "g"]
  ; code=
      (fun locals ->
        let _ = store_name locals (mk_string "wrapper0.<locals>.f.<locals>.c") "__qualname__" in
        (* store_name 2, with co_names[2] = "__qualname__" *)
        let _ = store_name locals (mk_int 4) "x" in
        (* store_name 3, with co_names[3] = "x" This goes straight to the locals name->value map *)
        let print = load_name locals globals "print" in
        (* load_name 4, with co_names[4] = "print" *)
        let x = load_name locals globals "x" in
        (* load_name 3 *)
        let _ = call_function print [x] in
        let y = load_classderef locals "y" in
        (* load_classderef 1, with co_freevars[1] = "y" *)
        let _ = call_function print [y] in
        (* cheat, didn't reload print or do newline *)
        let x = load_closure locals "x" in
        (* load_closure 0, with co_freevars[0] = "x" This gets the value from freevars[0]*)
        let y = load_closure locals "y" in
        (* load_closure 1 *)
        let g =
          make_function globals [x; y] g_code (mk_string "wrapper0.<locals>.f.<locals>.c.g")
        in
        let _ = store_name locals g "g" in
        (* store_name 5, with co_names[5] = "g" *)
        mk_int 0 ) }


let f_code globals =
  { co_freevars= []
  ; co_varnames= ["x"; "y"; "c"]
  ; co_cellvars= ["x"; "y"]
  ; co_argcount= 2
  ; co_names= ["print"]
  ; code=
      (fun locals ->
        let print = load_name locals globals "print" in
        (* load_global 0, see co_names *)
        let x = load_deref locals "x" in
        (* load_deref 0 with *both* co_cellvars[0] and co_varnames[0] = "x" *)
        (* Note that these load_derefs are for arguments - this is why we have to have sometimes create cells when passing args *)
        let _ = call_function print [x] in
        let y = load_deref locals "y" in
        (* load_deref 1 with similar *)
        let _ = call_function print [y] in
        let x = load_closure locals "x" in
        let y = load_closure locals "y" in
        let c_clos = make_function globals [x; y] c_code (mk_string "c") in
        let c = build_class c_clos (mk_string "c") [] in
        let _ = store_fast locals c "c" in
        (* store_fast 2, with co_varnames[2]="c", though this can be store_deref when I tweak the code a bit *)
        load_fast locals "c" ) }


let wrapper0_code globals =
  { co_freevars= []
  ; co_cellvars= []
  ; co_varnames= ["f"]
  ; co_argcount= 0
  ; co_names= ["print"; "g"]
  ; code=
      (fun locals ->
        let f = make_function globals [] f_code (mk_string "wrapper0.<locals>.f") in
        let _ = store_fast locals f "f" in
        let f = load_fast locals "f" in
        let res = call_function f [mk_int 0; mk_int 1] in
        let obj = call_function res [] in
        let m = load_method obj "g" in
        let _ = call_method m [] in
        mk_int 0 ) }


(* default outer globals map - or should this be in builtins? *)
let globals =
  let bindings = newbindings () in
  StringHashtble.replace bindings "print" python_print_function ;
  bindings


(* test some code with no args or free variables and print the result *)
let test wrapper =
  let code_obj = wrapper globals in
  let res = code_obj.code {fastlocals= newbindings (); locals= newbindings ()} in
  F.printf "%a@\n" pp_pval res


let%expect_test _ =
  printf "pysem\n" ;
  test wrapper_code ;
  printf "wrapper2\n" ;
  test wrapper2_code ;
  printf "wrapper3\n" ;
  test wrapper3_code ;
  printf "wrapper5\n" ;
  test wrapper5_code ;
  printf "wrapper0 from full monty paper\n" ;
  test wrapper0_code ;
  [%expect
    {|
      pysem
      wrapper2
      [class=Builtin; mval=3; dict={ }]
      [class=Builtin; mval=0; dict={ }]
      [class=Builtin; mval=assigned; dict={ }]
      [class=Builtin; mval=reassigned; dict={ }]
      [class=Builtin; mval=0; dict={ }]
      wrapper3
      [class=Builtin; mval=assigned; dict={ }]
      [class=Builtin; mval=reassigned; dict={ }]
      [class=Builtin; mval=0; dict={ }]
      wrapper5
      [class=Builtin; mval=3; dict={ }]
      [class=Builtin; mval=0; dict={ }]
      wrapper0 from full monty paper
      [class=Builtin; mval=0; dict={ }]
      [class=Builtin; mval=1; dict={ }]
      [class=Builtin; mval=4; dict={ }]
      [class=Builtin; mval=1; dict={ }]
      [class=Builtin; mval=0; dict={ }]
      [class=Builtin; mval=1; dict={ }]
      [class=Builtin; mval=0; dict={ }] |}]
