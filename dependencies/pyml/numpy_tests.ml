let () =
  Pyml_tests_common.add_test ~title:"of_bigarray"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let array = [| 1.; 2. |] in
          let array1 =
            Bigarray.Array1.of_array (Bigarray.float64) (Bigarray.c_layout) array in
          let bigarray = Bigarray.genarray_of_array1 array1 in
          let a = Numpy.of_bigarray bigarray in
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
          assert (Bigarray.Array1.get array1 0 = 42.);
          assert (Bigarray.Array1.get array1 1 = 43.);
          Pyml_tests_common.Passed
        end)

let () =
  Pyml_tests_common.add_test ~title:"of_bigarray2"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let array = [| [| 1.; 2.; 3. |]; [| -1.23; Float.nan; 2.72 |] |] in
          let array2 =
            Bigarray.Array2.of_array (Bigarray.float64) (Bigarray.c_layout) array in
          let bigarray = Bigarray.genarray_of_array2 array2 in
          let a = Numpy.of_bigarray bigarray in
          let m = Py.Import.add_module "test" in
          Py.Module.set m "array" a;
          assert (Py.Run.simple_string "
from test import array
import numpy

assert list(array.shape) == [2, 3]
numpy.testing.assert_almost_equal(array[0], [1, 2, 3])
assert(numpy.isnan(array[1, 1]))
array[0, 0] = 42.
array[0, 1] = 43.
array[1, 1] = 1.
");
          assert (Bigarray.Array2.get array2 0 0 = 42.);
          assert (Bigarray.Array2.get array2 0 1 = 43.);
          assert (Bigarray.Array2.get array2 1 1 = 1.);
          Pyml_tests_common.Passed
        end)

let () =
  Pyml_tests_common.add_test ~title:"to_bigarray"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let m = Py.Import.add_module "test" in
          let callback arg =
            let bigarray =
              Numpy.to_bigarray Bigarray.nativeint Bigarray.c_layout arg.(0) in
            assert (Bigarray.Genarray.dims bigarray = [| 4 |]);
            let array1 = Bigarray.array1_of_genarray bigarray in
            assert (Bigarray.Array1.get array1 0 = 0n);
            assert (Bigarray.Array1.get array1 1 = 1n);
            assert (Bigarray.Array1.get array1 2 = 2n);
            assert (Bigarray.Array1.get array1 3 = 3n);
            Py.none in
          Py.Module.set m "callback" (Py.Callable.of_function callback);
          assert (Py.Run.simple_string "
from test import callback
import numpy
callback(numpy.array([0,1,2,3]))
");
          Pyml_tests_common.Passed
        end)

let assert_almost_eq ?(eps = 1e-7) f1 f2 =
  if Float.abs (f1 -. f2) > eps then
    failwith (Printf.sprintf "%f <> %f" f1 f2)

let () =
  Pyml_tests_common.add_test ~title:"to_bigarray2"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let m = Py.Import.add_module "test" in
          let callback arg =
            let bigarray =
              Numpy.to_bigarray Bigarray.float32 Bigarray.c_layout arg.(0) in
            assert (Bigarray.Genarray.dims bigarray = [| 2; 4 |]);
            let array2 = Bigarray.array2_of_genarray bigarray in
            let assert_almost_eq i j v =
              assert_almost_eq (Bigarray.Array2.get array2 i j) v in
            let assert_is_nan i j =
              let v = Bigarray.Array2.get array2 i j in
              assert (Float.is_nan v) in
            assert_almost_eq 0 0 0.12;
            assert_almost_eq 0 1 1.23;
            assert_almost_eq 0 2 2.34;
            assert_almost_eq 0 3 3.45;
            assert_almost_eq 1 0 (-1.);
            assert_is_nan 1 1;
            assert_almost_eq 1 2 1.;
            assert_almost_eq 1 3 0.;
            Py.none in
          Py.Module.set m "callback" (Py.Callable.of_function callback);
          assert (Py.Run.simple_string "
from test import callback
import numpy
callback(numpy.array([[0.12,1.23,2.34,3.45],[-1.,numpy.nan,1.,0.]], dtype=numpy.float32))
");
          Pyml_tests_common.Passed
        end)

let assert_invalid_argument f =
  try
    let () = f () in
    assert false
  with Invalid_argument _ ->
    ()

let () =
  Pyml_tests_common.add_test ~title:"to_bigarray invalid type"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          assert_invalid_argument (fun () ->
            ignore (Numpy.to_bigarray Float64 C_layout Py.none));
          assert_invalid_argument (fun () ->
            ignore (Numpy.to_bigarray Float64 C_layout (Py.Int.of_int 0)));
          let array =
            Numpy.of_bigarray (Bigarray.genarray_of_array1 (
              Bigarray.Array1.of_array (Bigarray.float64) (Bigarray.c_layout)
                [| 1.; 2. |])) in
          ignore (Numpy.to_bigarray Float64 C_layout array);
          assert_invalid_argument (fun () ->
            ignore (Numpy.to_bigarray Float32 C_layout array));
          assert_invalid_argument (fun () ->
            ignore (Numpy.to_bigarray Float64 Fortran_layout array));
          Pyml_tests_common.Passed
        end)

let () =
  Pyml_tests_common.add_test ~title:"to_bigarray_k"
    (fun () ->
      if Py.Import.try_import_module "numpy" = None then
        Pyml_tests_common.Disabled "numpy is not available"
      else
        begin
          let m = Py.Import.add_module "test" in
          let callback arg =
            let k { Numpy.kind; layout; array } =
              assert (Numpy.compare_kind kind Bigarray.nativeint = 0);
              assert (Numpy.compare_layout layout Bigarray.c_layout = 0);
              let bigarray =
                Option.get (Numpy.check_kind_and_layout
                  Bigarray.nativeint Bigarray.c_layout array) in
              assert (Bigarray.Genarray.dims bigarray = [| 4 |]);
              let array1 = Bigarray.array1_of_genarray bigarray in
              assert (Bigarray.Array1.get array1 0 = 0n);
              assert (Bigarray.Array1.get array1 1 = 1n);
              assert (Bigarray.Array1.get array1 2 = 2n);
              assert (Bigarray.Array1.get array1 3 = 3n) in
            Numpy.to_bigarray_k { Numpy.f = k } arg.(0);
            Py.none in
          Py.Module.set m "callback" (Py.Callable.of_function callback);
          assert (Py.Run.simple_string "
from test import callback
import numpy
callback(numpy.array([0,1,2,3]))
");
          Pyml_tests_common.Passed
        end)

let () =
  if not !Sys.interactive then
    Pyml_tests_common.main ()
