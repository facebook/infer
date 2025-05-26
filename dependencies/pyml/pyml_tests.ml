let () =
  Pyml_tests_common.add_test
    ~title:"version"
    (fun () ->
      Printf.printf "Python version %s\n%!" (Py.version ());
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"library version"
    (fun () ->
      Printf.printf "Python library version %s\n%!" (Py.get_version ());
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"hello world"
    (fun () ->
      assert (Py.Run.simple_string "print('Hello world!')");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"class"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let value_obtained = ref None in
      let callback arg =
        value_obtained := Some (Py.String.to_string (Py.Tuple.get_item arg 1));
        Py.none in
      let c =
        Py.Class.init "myClass"
          ~methods:[("callback", Py.Callable.of_function_as_tuple callback)] in
      Py.Module.set m "myClass" c;
      assert (Py.Run.simple_string "
from test import myClass
myClass().callback('OK')
");
      assert (!value_obtained = Some "OK");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"empty tuple"
    (fun () ->
      assert (Py.Tuple.create 0 = Py.Tuple.empty);
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"make tuple"
    (fun () ->
      assert
        (Py.Tuple.to_singleton (Py.Tuple.singleton (Py.Long.of_int 0))
           = Py.Long.of_int 0);
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"module get/set/remove"
    (fun () ->
      let m = Py.Module.create "test" in
      Py.Module.set m "test" Py.none;
      assert (Py.Module.get m "test" = Py.none);
      Py.Module.remove m "test";
      begin
        try
          ignore (Py.Module.get m "test");
          Pyml_tests_common.Failed "Should have been removed"
        with Py.E _ -> Pyml_tests_common.Passed
      end)

let () =
  Pyml_tests_common.add_test
    ~title:"capsule"
    (fun () ->
      let (wrap, unwrap) = Py.Capsule.make "string" in
      let m = Py.Import.add_module "test" in
      let pywrap args =
        let s = Py.String.to_string args.(0) in
        wrap s in
      let pyunwrap args =
        let s = unwrap args.(0) in
        Py.String.of_string s in
      Py.Module.set_function m "wrap" pywrap;
      Py.Module.set_function m "unwrap" pyunwrap;
      assert (Py.Run.simple_string "
from test import wrap, unwrap
x = wrap('OK')
print('Capsule type: {0}'.format(x))
assert unwrap(x) == 'OK'
");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"capsule-conversion-error"
    (fun () ->
      let ref_str = "foobar" in
      let ref_pair1 = (3.141592, 42) in
      let ref_pair2 = (2.71828182846, 42) in
      let (wrap_str, unwrap_str) = Py.Capsule.make "string-1" in
      let (wrap_pair, unwrap_pair) = Py.Capsule.make "pair-1" in
      let s = wrap_str ref_str in
      let p1 = wrap_pair ref_pair1 in
      let p2 = wrap_pair ref_pair2 in
      assert (unwrap_str s = ref_str);
      assert (unwrap_pair p1 = ref_pair1);
      assert (unwrap_pair p2 = ref_pair2);
      let unwrap_failed =
        try
          ignore (unwrap_pair s : float * int);
          false
        with _ -> true
      in
      assert unwrap_failed;
      let unwrap_failed =
        try
          ignore (unwrap_pair (Py.Long.of_int 42) : float * int);
          false
        with _ -> true
      in
      assert unwrap_failed;
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"exception"
    (fun () ->
      try
        let _ = Py.Run.eval ~start:Py.File "
raise Exception('Great')
" in
        Pyml_tests_common.Failed "uncaught exception"
      with Py.E (_, value) ->
        assert (Py.Object.to_string value = "Great");
        Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"ocaml exception"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let mywrap _ =
        raise (Py.Err (Py.Err.Exception, "Great")) in
      Py.Module.set_function m "mywrap" mywrap;
      assert (Py.Run.simple_string "
from test import mywrap
try:
    mywrap()
    raise Exception('No exception raised')
except Exception as err:
    assert str(err) == \"Great\"
");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"ocaml exception with traceback"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let traceback = [
        { Py.Traceback.filename = "file1.ml";
          function_name = "func1";
          line_number = 1};
        { Py.Traceback.filename = "file2.ml";
          function_name = "func2";
          line_number = 2}
      ] in
      let mywrap _ =
        raise (Py.Err_with_traceback (Py.Err.Exception, "Great", traceback)) in
      Py.Module.set_function m "mywrap" mywrap;
      assert (Py.Run.simple_string "
from test import mywrap
import sys
import traceback
try:
    mywrap()
    raise Exception('No exception raised')
except Exception as err:
    if sys.version_info.major == 3 and sys.version_info.minor >= 7:
        if sys.version_info.minor >= 11:
            filenames = [
                f.filename for f in
                traceback.StackSummary.extract(
                    traceback.walk_tb(err.__traceback__))]
        else:
            filenames = [
                f.filename for f in traceback.extract_tb(err.__traceback__)]
        assert filenames == ['<string>', 'file2.ml', 'file1.ml']
    assert str(err) == \"Great\"
");
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"restore with null"
    (fun () ->
      try
        let _ = Py.Run.eval ~start:Py.File "
raise Exception('Great')
" in
        Pyml_tests_common.Failed "uncaught exception"
      with Py.E (_, value) -> begin
        assert (Py.Object.to_string value = "Great");
        match Py.Err.fetched () with
        | None -> Pyml_tests_common.Failed "unexpected none"
        | Some (err, _args, _traceback) ->
            (* Test that using [Py.Err.restore] on null works fine. *)
            Py.Err.restore err Py.null Py.null;
            Py.Err.clear ();
            Pyml_tests_common.Passed
    end)

let () =
  Pyml_tests_common.add_test
    ~title:"ocaml other exception"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let mywrap _ = raise Exit in
      Py.Module.set_function m "mywrap" mywrap;
      try
        ignore (Py.Run.eval ~start:File "
from test import mywrap
try:
    mywrap()
except Exception as err:
    raise Exception('Should not be caught by Python')
");
        Pyml_tests_common.Failed "Uncaught exception"
      with Exit ->
        Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"run file with filename"
    (fun () ->
      let result = Pyutils.with_temp_file "print(\"Hello, world!\")"
        begin fun file _channel ->
         Py.Run.load (Py.Filename file) "test.py"
        end in
      if result = Py.none then
        Pyml_tests_common.Passed
      else
        let result_str = Py.Object.to_string result in
        let msg = Printf.sprintf "Result None expected but got %s" result_str in
        Pyml_tests_common.Failed msg
    )

let () =
  Pyml_tests_common.add_test
    ~title:"run file with channel"
    (Pyml_tests_common.enable_only_on_unix
       (fun () ->
         let result = Pyutils.with_temp_file "print(\"Hello, world!\")"
           begin fun _file channel ->
           Py.Run.load (Py.Channel channel) "test.py"
           end in
         if result = Py.none then
           Pyml_tests_common.Passed
         else
           let result_str = Py.Object.to_string result in
           let msg = Printf.sprintf "Result None expected but got %s" result_str in
           Pyml_tests_common.Failed msg
       )
    )

let () =
  Pyml_tests_common.add_test
    ~title:"boolean"
    (fun () ->
      try
        if not (Py.Bool.to_bool (Py.Run.eval "True")) then
          Pyml_tests_common.Failed "true is false"
        else if Py.Bool.to_bool (Py.Run.eval "False") then
          Pyml_tests_common.Failed "false is true"
        else
          Pyml_tests_common.Passed;
      with Py.E (_, value) ->
        Pyml_tests_common.Failed (Py.Object.to_string value))

let () =
  Pyml_tests_common.add_test
    ~title:"reinitialize"
    (fun () ->
      Gc.full_major ();
      Py.finalize ();
      begin
        try
          assert (Py.Run.simple_string "not initialized");
          raise Exit
        with
          Failure _ -> ()
        | Exit -> failwith "Uncaught not initialized"
      end;
      let (version, minor) = !Pyml_tests_common.use_version in
      Py.initialize ~verbose:true ?version ?minor ();
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"string conversion error"
    (fun () ->
      try
        let _ = Py.String.to_string (Py.Long.of_int 0) in
        Pyml_tests_common.Failed "uncaught exception"
      with
        Py.E (_, value) ->
          Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value);
          Pyml_tests_common.Passed
      | Failure s ->
          Printf.printf "Caught failure: %s\n%!" s;
          Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"float conversion error"
    (fun () ->
      try
        let _ = Py.Float.to_float (Py.String.of_string "a") in
        Pyml_tests_common.Failed "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value);
        Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"long conversion error"
    (fun () ->
      try
        let _ = Py.Long.to_int (Py.String.of_string "a") in
        Pyml_tests_common.Failed "uncaught exception"
      with Py.E (_, value) ->
        Printf.printf "Caught exception: %s\n%!" (Py.Object.to_string value);
        Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"iterators"
    (fun () ->
      let iter = Py.Object.get_iter (Py.Run.eval "['a','b','c']") in
      let list = Py.Iter.to_list_map Py.String.to_string iter in
      assert (list = ["a"; "b"; "c"]);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"Iterator.create"
    (fun () ->
      let m = Py.Import.add_module "test" in
      let iter = Py.Iter.of_list_map Py.Int.of_int [3; 1; 4; 1; 5] in
      Py.Module.set m "ocaml_iterator" iter;
      assert (Py.Run.simple_string "
from test import ocaml_iterator
res = 0
for v in ocaml_iterator: res += v
");
      let main = Py.Module.get_dict (Py.Import.add_module "__main__") in
      let res = Py.Dict.find_string main "res" in
      assert (Py.Int.to_int res = 14);
      let iter = Py.Iter.of_list_map Py.String.of_string ["a"; "b"; "c"] in
      let list = Py.Iter.to_list_map Py.String.to_string iter in
      assert (list = ["a"; "b"; "c"]);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"Iterator.create_call"
    (fun () ->
      let iter_of_list python_of list =
        let list = ref list in
        let next () =
          match !list with
          | [] -> None
          | p :: q ->
              list := q;
              Some (python_of p)
        in
        Py.Iter.create_call next
      in
      let m = Py.Import.add_module "test" in
      let iter = iter_of_list Py.Int.of_int [3; 1; 4; 1; 5] in
      Py.Module.set m "ocaml_iterator2" iter;
      assert (Py.Run.simple_string "
from test import ocaml_iterator2
res = 0
for v in ocaml_iterator2: res += v
");
      let main = Py.Module.get_dict (Py.Import.add_module "__main__") in
      let res = Py.Dict.find_string main "res" in
      assert (Py.Int.to_int res = 14);
      let iter = iter_of_list Py.String.of_string ["a"; "b"; "c"] in
      let list = Py.Iter.to_list_map Py.String.to_string iter in
      assert (list = ["a"; "b"; "c"]);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"Dict.iter"
    (fun () ->
      let dict = Py.Dict.create () in
      for i = 0 to 9 do
        Py.Dict.set_item_string dict (string_of_int i) (Py.Long.of_int i)
      done;
      let table = Array.make 10 None in
      Py.Dict.iter begin fun key value ->
        let index = Py.Long.to_int value in
        assert (table.(index) = None);
        table.(index) <- Some (Py.String.to_string key)
      end dict;
      Array.iteri begin fun i v ->
        match v with
          None -> failwith "None!"
        | Some v' -> assert (i = int_of_string v')
      end table;
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"unicode"
    (fun () ->
      let codepoints = [| 8203; 127; 83; 2384; 0; 12 |] in
      let python_string = Py.String.of_unicode codepoints in
      let ocaml_string = Py.String.to_string python_string in
      let python_string' = Py.String.decode_UTF8 ocaml_string in
      let codepoints' = Py.String.to_unicode python_string' in
      assert (codepoints = codepoints');
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"interactive loop"
    (Pyml_tests_common.enable_only_on_unix (fun () ->
      Pyutils.with_stdin_from_string "42"
        Py.Run.interactive ();
      assert (Py.Long.to_int (Py.last_value ()) = 42);
      Pyml_tests_common.Passed))

let () =
  Pyml_tests_common.add_test
    ~title:"IPython"
    (Pyml_tests_common.enable_only_on_unix
       (Py.Run.frame (Pyutils.with_stdin_from_string "exit" (fun () ->
         if Py.Import.try_import_module "IPython" = None then
           Pyml_tests_common.Disabled "IPython is not available"
         else
           begin
             Py.Run.ipython ~frame:false ();
             Pyml_tests_common.Passed
           end))))

let () =
  Pyml_tests_common.add_test
    ~title:"Marshal"
    (fun () ->
      let v = Py.Long.of_int 42 in
      let m = Py.Marshal.dumps v in
      let v' = Py.Marshal.loads m in
      assert (Py.Long.to_int v' = 42);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"Py.List.of_list"
    (fun () ->
      let v = Py.List.of_list [Py.Long.of_int 42] in
      assert (Py.List.length v = 1);
      assert (Py.Long.to_int (Py.List.get v 0) = 42);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"Py.List.sort"
    (fun () ->
      let pi_digits = [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5; 8 ] in
      let v = Py.List.of_list [] in
      assert (Py.List.length v = 0);
      let count = Py.Object.call_method v "count" [|Py.Long.of_int 1|] in
      assert (Py.Long.to_int count = 0);
      List.iter (fun i ->
          ignore (Py.Object.call_method v "append" [|Py.Long.of_int i|]))
        pi_digits;
      let count = Py.Object.call_method v "count" [|Py.Long.of_int 1|] in
      assert (Py.Long.to_int count = 2);
      assert (Py.List.length v = List.length pi_digits);
      let _ = Py.Object.call_method v "sort" [||] in
      let sorted_digits = List.map Py.Int.to_int (Py.List.to_list v) in
      assert (sorted_digits = List.sort compare pi_digits);
      (* No `clear' method in lists in Python 2 *)
      if Py.version_major () >= 3 then
        begin
          let _ = Py.Object.call_method v "clear" [||] in
          assert (Py.List.length v = 0)
        end;
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test ~title:"array"
    (fun () ->
      let array = [| 1; 2 |] in
      let a = Py.Array.of_array Py.Long.of_int Py.Long.to_int array in
      let m = Py.Import.add_module "test" in
      Py.Module.set m "array" a;
      assert (Py.Run.simple_string "
from test import array
assert len(array) == 2
assert array[0] == 1
assert array[1] == 2
array[0] = 42
array[1] = 43
copy = []
for x in array:
  copy.append(x)
assert copy == [42, 43]
");
      assert (array.(0) = 42);
      assert (array.(1) = 43);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test ~title:"numpy"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let array = Array.Floatarray.create 2 in
          Array.Floatarray.set array 0 1.;
          Array.Floatarray.set array 1 2.;
          let a = Py.Array.numpy array in
          let m = Py.Import.add_module "test" in
          Py.Module.set m "array" a;
          assert (Py.Run.simple_string "
from test import array
assert len(array) == 2
assert array[0] == 1.
assert array[1] == 2.
array[0] = 42.
array[1] = 43.
");
          assert (Array.Floatarray.get array 0 = 42.);
          assert (Array.Floatarray.get array 1 = 43.);
          Pyml_tests_common.Passed
        end)

let () =
  Pyml_tests_common.add_test ~title:"numpy crunch"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let array = Float.Array.init 0x10000 float_of_int in
          let numpy_array = Py.Array.numpy array in
          let add =
            Py.Module.get_function (Py.Import.import_module "numpy") "add" in
          let rec crunch numpy_array n =
            if n <= 0 then
              numpy_array
            else
              let array =
                Float.Array.map_from_array Fun.id
                  (Py.Sequence.to_array_map Py.Float.to_float
                     (add [| numpy_array; numpy_array |])) in
              crunch (Py.Array.numpy array) (pred n) in
          ignore (crunch (Py.Array.numpy array) 0x100);
          assert (Float.Array.length array = 0x10000);
          for i = 0 to 0x10000 - 1 do
            assert (Float.Array.get array i = float_of_int i)
          done;
          Float.Array.set array 1 42.;
          assert (Py.Float.to_float (Py.Sequence.get numpy_array 1) = 42.);
          Pyml_tests_common.Passed
        end)

let () =
  Pyml_tests_common.add_test
    ~title:"none"
    (fun () ->
      let none = Py.none in
      assert (none = Py.none);
      assert (Py.is_none none);
      assert (Py.Type.get none = None);
      let none = Py.Run.eval "None" in
      assert (none = Py.none);
      assert (Py.is_none none);
      assert (Py.Type.get none = None);
      let not_none = Py.Long.of_int 42 in
      assert (not_none <> Py.none);
      assert (not (Py.is_none not_none));
      assert (Py.Type.get not_none <> None);
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"docstring"
    (fun () ->
      Gc.full_major ();
      let fn =
        let docstring = Printf.sprintf "test%d" 42 in
        Py.Callable.of_function ~docstring (fun _ -> Py.none)
      in
      Gc.full_major ();
      let other_string = Printf.sprintf "test%d" 43 in
      let doc = Py.Object.get_attr_string fn "__doc__" in
      begin
        match doc with
          None -> failwith "None!"
        | Some doc -> assert (Py.String.to_string doc = "test42")
      end;
      ignore other_string;
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"function-name"
    (fun () ->
      let run make_name expect_name =
        Gc.full_major ();
        let fn =
          let name = make_name () in
          Py.Callable.of_function ?name (fun _ -> Py.none)
        in
        Gc.full_major ();
        let other_string = Printf.sprintf "test%d" 43 in
        let name = Py.Object.get_attr_string fn "__name__" in
        begin
          match name with
            None -> failwith "None!"
          | Some doc -> assert (Py.String.to_string doc = expect_name)
        end;
        ignore other_string
      in
      run (fun () -> Some (Printf.sprintf "test%d" 42)) "test42";
      run (fun () -> None) "anonymous_closure";
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"is-instance"
    (fun () ->
      let forty_two = Py.Int.of_int 42 in
      let forty_two_str = Py.String.of_string "42" in
      let int = Py.Dict.find_string (Py.Eval.get_builtins ()) "int" in
      assert (Py.Object.is_instance forty_two int);
      assert (not (Py.Object.is_instance forty_two_str int));
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"is-subclass"
    (fun () ->
      let int = Py.Dict.find_string (Py.Eval.get_builtins ()) "int" in
      let cls1 = Py.Class.init ~parents:[int] "cls1" in
      let cls2 = Py.Class.init ~parents:[cls1] "cls2" in
      assert (Py.Object.is_subclass cls1 int);
      assert (not (Py.Object.is_subclass int cls1));
      assert (Py.Object.is_subclass cls2 cls1);
      assert (not (Py.Object.is_subclass cls1 cls2));
      assert (Py.Object.is_subclass cls2 int);
      assert (not (Py.Object.is_subclass int cls2));
      Pyml_tests_common.Passed
    )

let () =
  Pyml_tests_common.add_test
    ~title:"Set"
    (fun () ->
      let set = Py.Set.create () in
      for i = 0 to 9 do
        Py.Set.add set (Py.Long.of_int i)
      done;
      assert (Py.Set.check set);
      assert (Py.Set.size set = 10);
      Py.Set.discard set (Py.Long.of_int 5);
      assert (Py.Set.size set = 9);
      let values = Py.Set.to_list_map Py.Long.to_int set in
      assert (values = [0; 1; 2; 3; 4; 6; 7; 8; 9]);
      let set' = Py.Set.copy set in
      Py.Set.add set' (Py.Long.of_int 42);
      Py.Set.add set' (Py.Long.of_int 42);
      assert (Py.Set.size set = 9);
      assert (Py.Set.size set' = 10);
      Pyml_tests_common.Passed)

let () =
  Pyml_tests_common.add_test
    ~title:"serialize"
    (fun () ->
      let value = Py.String.of_string "hello" in
      let pickled = Marshal.to_string value [] in
      let unpickled = Marshal.from_string pickled 0 in
      assert (Py.String.to_string unpickled = "hello");
      Pyml_tests_common.Passed)

let () =
  if not !Sys.interactive then
    Pyml_tests_common.main ()
