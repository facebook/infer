#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include "pyml_stubs.h"

value
pyml_wrap(PyObject *object, bool steal);

PyObject *
pyml_unwrap(value v);

struct numpy_custom_operations {
    struct custom_operations ops;
    PyObject *obj;
};

static void numpy_finalize(value v)
{
    struct numpy_custom_operations *ops =
        (struct numpy_custom_operations *) Custom_ops_val(v);
    Py_DECREF(ops->obj);
    free(ops);
}

CAMLprim value
pyarray_of_bigarray_wrapper(
  value numpy_api_ocaml, value bigarray_type_ocaml, value bigarray_ocaml)
{
    CAMLparam3(numpy_api_ocaml, bigarray_type_ocaml, bigarray_ocaml);
    pyml_assert_initialized();
    PyObject *c_api = pyml_unwrap(numpy_api_ocaml);
    void **PyArray_API = pyml_get_pyarray_api(c_api);
    PyObject *(*PyArray_New)
        (PyTypeObject *, int, npy_intp *, int, npy_intp *, void *, int, int,
         PyObject *) = PyArray_API[93];
    int nd = Caml_ba_array_val(bigarray_ocaml)->num_dims;
    npy_intp *dims = malloc(nd * sizeof(npy_intp));
    int i;
    for (i = 0; i < nd; i++) {
        dims[i] = Caml_ba_array_val(bigarray_ocaml)->dim[i];
    }
    int type_num;
    intnat flags = Caml_ba_array_val(bigarray_ocaml)->flags;
    switch (flags & CAML_BA_KIND_MASK) {
    case CAML_BA_FLOAT32:
        type_num = NPY_FLOAT;
        break;
    case CAML_BA_FLOAT64:
        type_num = NPY_DOUBLE;
        break;
    case CAML_BA_SINT8:
        type_num = NPY_BYTE;
        break;
    case CAML_BA_UINT8:
        type_num = NPY_UBYTE;
        break;
    case CAML_BA_SINT16:
        type_num = NPY_SHORT;
        break;
    case CAML_BA_UINT16:
        type_num = NPY_USHORT;
        break;
    case CAML_BA_INT32:
        type_num = NPY_INT;
        break;
    case CAML_BA_INT64:
        type_num = NPY_LONGLONG;
        break;
    case CAML_BA_CAML_INT:
        caml_failwith("Caml integers are unsupported for NumPy array");
        break;
    case CAML_BA_NATIVE_INT:
        type_num = NPY_LONG;
        break;
    case CAML_BA_COMPLEX32:
        type_num = NPY_CFLOAT;
        break;
    case CAML_BA_COMPLEX64:
        type_num = NPY_CDOUBLE;
        break;
#ifdef CAML_BA_CHAR /* introduced in 4.02.0 */
    case CAML_BA_CHAR:
        type_num = NPY_CHAR;
        break;
#endif
    default:
        caml_failwith("Unsupported bigarray kind for NumPy array");
    }
    int np_flags;
    switch (flags & CAML_BA_LAYOUT_MASK) {
    case CAML_BA_C_LAYOUT:
        np_flags = NPY_ARRAY_CARRAY;
        break;
    case CAML_BA_FORTRAN_LAYOUT:
        np_flags = NPY_ARRAY_FARRAY;
        break;
    default:
        caml_failwith("Unsupported bigarray layout for NumPy array");
    }
    void *data = Caml_ba_data_val(bigarray_ocaml);
    PyTypeObject (*PyArray_SubType) =
        (PyTypeObject *) pyml_unwrap(bigarray_type_ocaml);
    PyObject *result = PyArray_New(
        PyArray_SubType, nd, dims, type_num, NULL, data, 0,
        np_flags, NULL);
    free(dims);
    CAMLreturn(pyml_wrap(result, true));
}

CAMLprim value
bigarray_of_pyarray_wrapper(
  value numpy_api_ocaml, value pyarray_ocaml)
{
    CAMLparam2(numpy_api_ocaml, pyarray_ocaml);
    CAMLlocal2(bigarray, result);
    pyml_assert_initialized();
    PyObject *array = pyml_unwrap(pyarray_ocaml);
    PyArrayObject_fields *fields =
      (PyArrayObject_fields *) pyobjectdescr(array);
    int nd = fields->nd;
    npy_intp *shape = fields->dimensions;
    intnat *dims = malloc(nd * sizeof(intnat));
    int i;
    for (i = 0; i < nd; i++) {
        dims[i] = shape[i];
    }
    int type = fields->descr->type_num;
    enum caml_ba_kind kind;
    switch (type) {
    case NPY_BYTE:
        kind = CAML_BA_SINT8;
        break;
    case NPY_UBYTE:
        kind = CAML_BA_UINT8;
        break;
    case NPY_SHORT:
        kind = CAML_BA_SINT16;
        break;
    case NPY_USHORT:
        kind = CAML_BA_UINT16;
        break;
    case NPY_INT:
        kind = CAML_BA_INT32;
        break;
    case NPY_LONG:
        kind = CAML_BA_NATIVE_INT;
        break;
    case NPY_LONGLONG:
        kind = CAML_BA_INT64;
        break;
    case NPY_FLOAT:
        kind = CAML_BA_FLOAT32;
        break;
    case NPY_DOUBLE:
        kind = CAML_BA_FLOAT64;
        break;
    case NPY_CFLOAT:
        kind = CAML_BA_COMPLEX32;
        break;
    case NPY_CDOUBLE:
        kind = CAML_BA_COMPLEX64;
        break;
    case NPY_CHAR:
#ifdef CAML_BA_CHAR /* introduced in 4.02.0 */
        kind = CAML_BA_CHAR;
#else
        kind = CAML_BA_UINT8;
#endif
        break;
    default:
        caml_failwith("Unsupported NumPy kind for bigarray");
    }
    int flags = fields->flags;
    enum caml_ba_layout layout;
    if (flags & NPY_ARRAY_C_CONTIGUOUS) {
        layout = CAML_BA_C_LAYOUT;
    }
    else if (flags & NPY_ARRAY_F_CONTIGUOUS) {
        layout = CAML_BA_FORTRAN_LAYOUT;
    }
    else {
        caml_failwith("Unsupported NumPy layout for bigarray");
    }
    void *data = fields->data;
    bigarray = caml_ba_alloc(kind | layout, nd, data, dims);
    free(dims);
    Py_INCREF(array);
    const struct custom_operations *oldops = Custom_ops_val(bigarray);
    struct numpy_custom_operations *newops = (struct numpy_custom_operations *)
        malloc(sizeof(struct numpy_custom_operations));
    newops->ops.identifier = oldops->identifier;
    newops->ops.finalize = numpy_finalize;
    newops->ops.compare = oldops->compare;
    newops->ops.hash = oldops->hash;
    newops->ops.serialize = oldops->serialize;
    newops->ops.deserialize = oldops->deserialize;
    newops->ops.compare_ext = oldops->compare_ext;
    newops->obj = array;
    Custom_ops_val(bigarray) = (struct custom_operations *) newops;
    result = caml_alloc_tuple(3);
    Store_field(result, 0, Val_int(kind));
    Store_field(result, 1, Val_int(layout == CAML_BA_FORTRAN_LAYOUT ? 1 : 0));
    Store_field(result, 2, bigarray);
    CAMLreturn(result);
}
