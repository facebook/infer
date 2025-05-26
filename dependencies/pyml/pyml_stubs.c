#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include "pyml_stubs.h"

static FILE* (*Python__Py_fopen)(const char* pathname, const char* mode);

static FILE* (*Python__Py_wfopen)(const wchar_t* pathname, const wchar_t* mode);

static void* xmalloc(size_t size) {
  void* p = malloc(size);
  if (!p) {
    caml_failwith("Virtual memory exhausted\n");
  }
  return p;
}

#ifdef _WIN32
#include <windows.h>

typedef HINSTANCE library_t;

static library_t open_library(const char* filename) {
  return LoadLibrary(filename);
}

static char* get_library_error() { return "Unable to load library"; }

static void close_library(library_t library) {
  if (!FreeLibrary(library)) {
    fprintf(stderr, "close_library.\n");
    exit(EXIT_FAILURE);
  }
}

static library_t get_default_library(void) { return GetModuleHandle(0); }

static void* find_symbol(library_t library, const char* name) {
  return GetProcAddress(library, name);
}

int unsetenv(const char* name) {
  size_t len = strlen(name);
  char* string = xmalloc(len + 2);
  int result;
  snprintf(string, len + 2, "%s=", name);
  result = _putenv(string);
  free(string);
  return result;
}

extern int win_CRT_fd_of_filedescr(value handle);

static FILE* file_of_file_descr(value file_descr, const char* mode) {
  CAMLparam1(file_descr);
  int fd = win_CRT_fd_of_filedescr(file_descr);
  FILE* result = _fdopen(_dup(fd), mode);
  CAMLreturnT(FILE*, result);
}
#else
#include <dlfcn.h>

typedef void* library_t;

static library_t open_library(const char* filename) {
  return dlopen(filename, RTLD_LAZY | RTLD_GLOBAL);
}

static char* get_library_error() { return dlerror(); }

void close_library(library_t filename) {
  if (dlclose(filename)) {
    fprintf(stderr, "close_library: %s.\n", dlerror());
    exit(EXIT_FAILURE);
  }
}

static library_t get_default_library(void) { return RTLD_DEFAULT; }

static void* find_symbol(library_t library, const char* name) {
  return dlsym(library, name);
}

static FILE* file_of_file_descr(value file_descr, const char* mode) {
  CAMLparam1(file_descr);
  int fd = Int_val(file_descr);
  FILE* result = fdopen(dup(fd), mode);
  CAMLreturnT(FILE*, result);
}
#endif

static void* Python27__PyObject_NextNotImplemented;

/* Global variables for the library */

/* version_major != 0 iff the library is initialized */
static int version_major;
static int version_minor;

static library_t library;

/* Functions that are special enough to deserved to be wrapped specifically */

/* Wrapped by pywrap_closure */
static PyObject* (*Python_PyCFunction_NewEx)(PyMethodDef*,
                                             PyObject*,
                                             PyObject*);

/* Wrapped by closure and capsule */
static void* (*Python27_PyCapsule_New)(void*,
                                       const char*,
                                       PyCapsule_Destructor);
static void* (*Python27_PyCapsule_GetPointer)(PyObject*, const char*);
static int (*Python27_PyCapsule_IsValid)(PyObject*, const char*);
static void* (*Python2_PyCObject_FromVoidPtr)(void*, void (*)(void*));
static void* (*Python2_PyCObject_AsVoidPtr)(PyObject*);

/* Hack for multi-arguments */
static PyObject* (*Python_PyObject_CallFunctionObjArgs)(PyObject*, ...);
static PyObject* (*Python_PyObject_CallMethodObjArgs)(PyObject*,
                                                      PyObject*,
                                                      ...);

/* Wrapped by PyErr_Fetch_wrapper */
static void (*Python_PyErr_Fetch)(PyObject**, PyObject**, PyObject**);
static void (*Python_PyErr_Restore)(PyObject*, PyObject*, PyObject*);
static void (*Python_PyErr_NormalizeException)(PyObject**,
                                               PyObject**,
                                               PyObject**);

/* Resolved differently between Python 2 and Python 3 */
static PyObject* Python__Py_FalseStruct;

/* Buffer and size */
static int (*Python_PyString_AsStringAndSize)(PyObject*, char**, Py_ssize_t*);
static int (*Python_PyObject_AsCharBuffer)(PyObject*,
                                           const char**,
                                           Py_ssize_t*);
static int (*Python_PyObject_AsReadBuffer)(PyObject*,
                                           const void**,
                                           Py_ssize_t*);
static int (*Python_PyObject_AsWriteBuffer)(PyObject*, void**, Py_ssize_t*);

/* Length argument */
static PyObject* (*Python_PyLong_FromString)(const char*, const char**, int);

/* Internal use only */
static void (*Python_PyMem_Free)(void*);

/* Generate traceback objects. */
static PyObject* (*Python_PyThreadState_Get)();
static PyObject* (*Python_PyFrame_New)(PyObject*,
                                       PyObject*,
                                       PyObject*,
                                       PyObject*);
static PyObject* (*Python_PyCode_NewEmpty)(const char*, const char*, int);

static enum UCS { UCS_NONE, UCS2, UCS4 } ucs;

/* Single instance of () */
static PyObject* tuple_empty;

#include "pyml.h"
#include <stdio.h>

static void* getcustom(value v) { return *((void**)Data_custom_val(v)); }

static void pydecref(value v) {
  if (getcustom(v)) {
    Py_DECREF((PyObject*)getcustom(v));
  }
}

static int rich_compare_bool_nofail(PyObject* o1, PyObject* o2, int opid) {
  int result = Python_PyObject_RichCompareBool(o1, o2, opid);
  if (result == -1) {
    Python_PyErr_Clear();
    result = 0;
  }
  return result;
}

static int pycompare(value v1, value v2) {
  int result;
  PyObject* o1 = getcustom(v1);
  PyObject* o2 = getcustom(v2);

  if (o1 && !o2)
    result = -1;
  else if (o2 && !o1)
    result = 1;
  else if (!o1 && !o2)
    result = 0;
  else if (version_major < 3)
    Python2_PyObject_Cmp(o1, o2, &result);
  else if (rich_compare_bool_nofail(o1, o2, Py_EQ))
    result = 0;
  else if (rich_compare_bool_nofail(o1, o2, Py_LT))
    result = -1;
  else if (rich_compare_bool_nofail(o1, o2, Py_GT))
    result = 1;
  else
    result = -1;

  return result;
}

static intnat pyhash(value v) {
  if (getcustom(v)) {
    intnat result = Python_PyObject_Hash((PyObject*)getcustom(v));
    if (result == -1) {
      Python_PyErr_Clear();
    }
    return result;
  } else {
    return 0;
  }
}

void pyml_assert_initialized() {
  if (!version_major) {
    caml_failwith("Run 'Py.initialize ()' first");
  }
}

/** Creates a Python tuple initialized with a single element given by the
    argument. The reference to the argument is stolen. */
static PyObject* singleton(PyObject* value) {
  PyObject* result = Python_PyTuple_New(1);
  if (!result) {
    caml_failwith("PyTuple_New");
  }
  if (Python_PyTuple_SetItem(result, 0, value)) {
    caml_failwith("PyTuple_SetItem");
  }
  return result;
}

static void pyserialize(value v, uintnat* bsize_32, uintnat* bsize_64) {
  pyml_assert_initialized();
  PyObject* value = getcustom(v);
  PyObject* pickle = Python_PyImport_ImportModule("pickle");
  if (pickle == NULL) {
    caml_failwith("Cannot import pickle");
  }
  PyObject* dumps = Python_PyObject_GetAttrString(pickle, "dumps");
  if (dumps == NULL) {
    caml_failwith("pickle.dumps unavailable");
  }
  PyObject* args = singleton(value);
  PyObject* bytes = Python_PyObject_Call(dumps, args, NULL);
  if (bytes == NULL) {
    caml_failwith("pickle.dumps failed");
  }
  Py_ssize_t size;
  char* contents;
  if (version_major >= 3) {
    size = Python3_PyBytes_Size(bytes);
    contents = (char*)Python3_PyBytes_AsString(bytes);
  } else {
    size = Python2_PyString_Size(bytes);
    contents = (char*)Python2_PyString_AsString(bytes);
  }
  caml_serialize_int_8(size);
  caml_serialize_block_1(contents, size);
  *bsize_32 = 4;
  *bsize_64 = 8;
  /*Py_DECREF(bytes);*/ /* reference stolen by args */
  Py_DECREF(args);
  Py_DECREF(dumps);
  Py_DECREF(pickle);
}

static uintnat pydeserialize(void* dst) {
  pyml_assert_initialized();
  Py_ssize_t size = caml_deserialize_uint_8();
  PyObject* bytes;
  char* contents;
  if (version_major >= 3) {
    bytes = Python3_PyBytes_FromStringAndSize(NULL, size);
    contents = (char*)Python3_PyBytes_AsString(bytes);
  } else {
    bytes = Python2_PyString_FromStringAndSize(NULL, size);
    contents = (char*)Python2_PyString_AsString(bytes);
  }
  caml_deserialize_block_1(contents, size);
  PyObject* pickle = Python_PyImport_ImportModule("pickle");
  if (pickle == NULL) {
    caml_failwith("Cannot import pickle");
  }
  PyObject* loads = Python_PyObject_GetAttrString(pickle, "loads");
  if (loads == NULL) {
    caml_failwith("pickle.loads unavailable");
  }
  PyObject* args = singleton(bytes);
  PyObject* value = Python_PyObject_Call(loads, args, NULL);
  if (value == NULL) {
    caml_failwith("pickle.loads failed");
  }
  *((PyObject**)dst) = value;
  /*Py_DECREF(bytes);*/ /* reference stolen by args */
  Py_DECREF(args);
  Py_DECREF(loads);
  Py_DECREF(pickle);
  return sizeof(PyObject*);
}

struct custom_operations pyops = {
    "PythonObject", pydecref, pycompare, pyhash, pyserialize, pydeserialize};

enum code { CODE_NULL, CODE_NONE, CODE_TRUE, CODE_FALSE, CODE_TUPLE_EMPTY };

static void* resolve(const char* symbol) {
  void* result = find_symbol(library, symbol);
  if (!result) {
    char* fmt = "Cannot resolve %s.\n";
    ssize_t size = snprintf(NULL, 0, fmt, symbol);
    char* msg = xmalloc(size + 1);
    snprintf(msg, size + 1, fmt, symbol);
    caml_failwith(msg);
  }
  return result;
}

static void* resolve_optional(const char* symbol) {
  return find_symbol(library, symbol);
}

value pyml_wrap(PyObject* object, bool steal) {
  CAMLparam0();
  CAMLlocal1(v);
  if (!object) {
    CAMLreturn(Val_int(CODE_NULL));
  }
  if (object == Python__Py_NoneStruct) {
    CAMLreturn(Val_int(CODE_NONE));
  }
  if (object == Python__Py_TrueStruct) {
    CAMLreturn(Val_int(CODE_TRUE));
  }
  if (object == Python__Py_FalseStruct) {
    CAMLreturn(Val_int(CODE_FALSE));
  }
  unsigned long flags =
      ((struct _typeobject*)pyobjectdescr(pyobjectdescr(object)->ob_type))
          ->tp_flags;
  if (flags & Py_TPFLAGS_TUPLE_SUBCLASS &&
      Python_PySequence_Length(object) == 0) {
    CAMLreturn(Val_int(CODE_TUPLE_EMPTY));
  }
  if (!steal) {
    Py_INCREF(object);
  }
  v = caml_alloc_custom(&pyops, sizeof(PyObject*), 100, 30000000);
  *((PyObject**)Data_custom_val(v)) = object;
  CAMLreturn(v);
}

PyObject* pyml_unwrap(value v) {
  if (Is_long(v))
    switch (Int_val(v)) {
      case CODE_NULL:
        return NULL;
      case CODE_NONE:
        return Python__Py_NoneStruct;
      case CODE_TRUE:
        return Python__Py_TrueStruct;
      case CODE_FALSE:
        return Python__Py_FalseStruct;
      case CODE_TUPLE_EMPTY:
        return tuple_empty;
    }

  return *((PyObject**)Data_custom_val(v));
}

/*
static value
pyml_wrap_compilerflags(PyCompilerFlags *flags)
{
    CAMLparam0();
    CAMLlocal2(ref, some);
    if (!flags) {
        CAMLreturn(Val_int(0));
    }
    else {
        ref = caml_alloc(0, 1);
        Store_field(ref, 0, Val_int(flags->cf_flags));
        some = caml_alloc(0, 1);
        Store_field(some, 0, ref);
        CAMLreturn(some);
    }
}
*/

static PyCompilerFlags* pyml_unwrap_compilerflags(value v) {
  CAMLparam1(v);
  if (Is_block(v)) {
    PyCompilerFlags* flags = malloc(sizeof(PyCompilerFlags));
    flags->cf_flags = Int_val(Field(Field(v, 0), 0));

    /* only useful for Python >= 3.8 */
    flags->cf_feature_version = version_minor;

    CAMLreturnT(PyCompilerFlags*, flags);
  } else {
    CAMLreturnT(PyCompilerFlags*, NULL);
  }
}

/*
static value
pyml_wrap_intref(int v)
{
    CAMLparam0();
    CAMLlocal1(ref);
    ref = caml_alloc(0, 1);
    Store_field(ref, 0, Val_int(v));
    CAMLreturn(ref);
}
*/

static int pyml_unwrap_intref(value v) {
  CAMLparam1(v);
  CAMLreturnT(int, Int_val(Field(v, 0)));
}

static void* unwrap_capsule(PyObject* obj, const char* type) {
  if (Python27_PyCapsule_GetPointer) {
    return Python27_PyCapsule_GetPointer(obj, type);
  } else {
    return Python2_PyCObject_AsVoidPtr(obj);
  }
}

static PyObject* wrap_capsule(void* ptr, char* type, void (*destr)(PyObject*)) {
  if (Python27_PyCapsule_New) {
    return Python27_PyCapsule_New(ptr, type, destr);
  } else {
    return Python2_PyCObject_FromVoidPtr(ptr, (void (*)(void*))destr);
  }
}

static PyObject* pycall_callback(PyObject* obj, PyObject* args) {
  CAMLparam0();
  CAMLlocal3(ml_out, ml_func, ml_args);
  PyObject* out;
  void* p = unwrap_capsule(obj, "ocaml-closure");
  if (!p) {
    Py_INCREF(Python__Py_NoneStruct);
    CAMLreturnT(PyObject*, Python__Py_NoneStruct);
  }
  ml_func = *(value*)p;
  ml_args = pyml_wrap(args, false);
  ml_out = caml_callback(ml_func, ml_args);
  out = pyml_unwrap(ml_out);
  Py_XINCREF(out);
  CAMLreturnT(PyObject*, out);
}

static PyObject* pycall_callback_with_keywords(PyObject* obj,
                                               PyObject* args,
                                               PyObject* keywords) {
  CAMLparam0();
  CAMLlocal4(ml_out, ml_func, ml_args, ml_keywords);
  PyObject* out;
  void* p = unwrap_capsule(obj, "ocaml-closure");
  if (!p) {
    Py_INCREF(Python__Py_NoneStruct);
    CAMLreturnT(PyObject*, Python__Py_NoneStruct);
  }
  ml_func = *(value*)p;
  ml_args = pyml_wrap(args, false);
  ml_keywords = pyml_wrap(keywords, false);
  ml_out = caml_callback2(ml_func, ml_args, ml_keywords);
  out = pyml_unwrap(ml_out);
  Py_XINCREF(out);
  CAMLreturnT(PyObject*, out);
}

static void caml_destructor(PyObject* v, const char* capsule_name) {
  value* valptr = (value*)unwrap_capsule(v, capsule_name);
  caml_remove_global_root(valptr);
  free(valptr);
}

static void camldestr_capsule(PyObject* v) {
  caml_destructor(v, "ocaml-capsule");
}

static PyObject* camlwrap_capsule(value val, void* aux_str, int size) {
  value* v = (value*)malloc(sizeof(value) + size);
  *v = val;
  memcpy((char*)v + sizeof(value), aux_str, size);
  caml_register_global_root(v);
  return wrap_capsule(v, "ocaml-capsule", camldestr_capsule);
}

static void* caml_aux(PyObject* obj) {
  value* v = (value*)unwrap_capsule(obj, "ocaml-closure");
  return (char*)v + sizeof(value);
}

void pyml_assert_python2() {
  if (version_major != 2) {
    pyml_assert_initialized();
    caml_failwith("Python 2 needed");
  }
}

void pyml_assert_ucs2() {
  if (ucs != UCS2) {
    pyml_assert_initialized();
    caml_failwith("Python with UCS2 needed");
  }
}

void pyml_assert_ucs4() {
  if (ucs != UCS4) {
    pyml_assert_initialized();
    caml_failwith("Python with UCS4 needed");
  }
}

void pyml_assert_python3() {
  if (version_major != 3) {
    pyml_assert_initialized();
    caml_failwith("Python 3 needed");
  }
}

void pyml_check_symbol_available(void* symbol, char* symbol_name) {
  if (!symbol) {
    char* fmt = "Symbol unavailable with this version of Python: %s.\n";
    ssize_t size = snprintf(NULL, 0, fmt, symbol_name);
    if (size < 0) {
      caml_failwith("Symbol unavailable with this version of Python.\n");
      return;
    }
    char* msg = xmalloc(size + 1);
    size = snprintf(msg, size + 1, fmt, symbol_name);
    if (size < 0) {
      caml_failwith("Symbol unavailable with this version of Python.\n");
      return;
    }
    caml_failwith(msg);
  }
}

void* deref_not_null(void* pointer) {
  if (pointer) {
    return *(void**)pointer;
  } else {
    return NULL;
  }
}

struct pyml_closure {
  value value;
  PyMethodDef method;
};

static char* anon_closure = "anonymous_closure";

static void camldestr_closure(PyObject* v) {
  struct pyml_closure* valptr = unwrap_capsule(v, "ocaml-closure");
  const char* ml_doc = valptr->method.ml_doc;
  const char* ml_name = valptr->method.ml_name;
  caml_remove_global_root((value*)valptr);
  free(valptr);
  free((void*)ml_doc);
  if (ml_name != anon_closure)
    free((void*)ml_name);
}

CAMLprim value pyml_wrap_closure(value name, value docstring, value closure) {
  CAMLparam3(name, docstring, closure);
  pyml_assert_initialized();
  PyMethodDef ml;
  PyObject* obj;
  PyMethodDef* ml_def;
  ml.ml_name = anon_closure;
  if (name != Val_int(0)) {
    ml.ml_name = strdup(String_val(Field(name, 0)));
  }
  if (Tag_val(closure) == 0) {
    ml.ml_flags = 1;
    ml.ml_meth = pycall_callback;
  } else {
    ml.ml_flags = 3;
    ml.ml_meth = (PyCFunction)pycall_callback_with_keywords;
  }
  ml.ml_doc = strdup(String_val(docstring));
  struct pyml_closure* v = malloc(sizeof(struct pyml_closure));
  v->value = Field(closure, 0);
  v->method = ml;
  caml_register_global_root(&v->value);
  obj = wrap_capsule(v, "ocaml-closure", camldestr_closure);
  ml_def = (PyMethodDef*)caml_aux(obj);
  PyObject* f = Python_PyCFunction_NewEx(ml_def, obj, NULL);
  Py_DECREF(obj);
  CAMLreturn(pyml_wrap(f, true));
}

int debug_build;

int trace_refs_build;

static void guess_debug_build() {
  PyObject* sysconfig = Python_PyImport_ImportModule("sysconfig");
  if (!sysconfig) {
    caml_failwith("Cannot import sysconfig");
  }
  PyObject* get_config_var =
      Python_PyObject_GetAttrString(sysconfig, "get_config_var");
  assert(get_config_var);
  PyObject* args;
  PyObject* py_debug;
  PyObject* debug_build_py;
  char* py_debug_str = "Py_DEBUG";
  if (version_major >= 3) {
    py_debug = Python3_PyUnicode_FromStringAndSize(py_debug_str, 8);
  } else {
    py_debug = Python2_PyString_FromStringAndSize(py_debug_str, 8);
  }
  assert(py_debug);
  args = singleton(py_debug);
  debug_build_py = Python_PyObject_Call(get_config_var, args, NULL);
  assert(debug_build_py);
  if (debug_build_py == Python__Py_NoneStruct) {
    debug_build = 0;
  } else {
    if (version_major >= 3) {
      debug_build = Python_PyLong_AsLong(debug_build_py);
    } else {
      debug_build = Python2_PyInt_AsLong(debug_build_py);
    }
    if (debug_build == -1) {
      caml_failwith("Cannot check for debug build");
    }
  }
  Py_DECREF(args);
  Py_DECREF(get_config_var);
  Py_DECREF(sysconfig);
}

CAMLprim value py_load_library(value filename_ocaml, value debug_build_ocaml) {
  CAMLparam2(filename_ocaml, debug_build_ocaml);
  if (Is_block(filename_ocaml)) {
    const char* filename = String_val(Field(filename_ocaml, 0));
    library = open_library(filename);
    if (!library) {
      caml_failwith(get_library_error());
    }
  } else {
    library = get_default_library();
  }
  Python_Py_GetVersion = find_symbol(library, "Py_GetVersion");
  if (!Python_Py_GetVersion) {
    caml_failwith("No Python symbol");
  }
  const char* version = Python_Py_GetVersion();
  version_major = version[0] - '0';
  version_minor = version[2] - '0';
  Python_PyCFunction_NewEx = resolve("PyCFunction_NewEx");
  if ((version_major == 2 && version_minor >= 7) || version_major >= 3) {
    Python27_PyCapsule_New = resolve("PyCapsule_New");
    Python27_PyCapsule_GetPointer = resolve("PyCapsule_GetPointer");
    Python27_PyCapsule_IsValid = resolve("PyCapsule_IsValid");
    Python27__PyObject_NextNotImplemented =
        resolve("_PyObject_NextNotImplemented");
  }
  Python_PyObject_CallFunctionObjArgs = resolve("PyObject_CallFunctionObjArgs");
  Python_PyObject_CallMethodObjArgs = resolve("PyObject_CallMethodObjArgs");
  Python_PyErr_Fetch = resolve("PyErr_Fetch");
  Python_PyErr_Restore = resolve("PyErr_Restore");
  Python_PyErr_NormalizeException = resolve("PyErr_NormalizeException");
  Python_PyObject_AsCharBuffer = resolve_optional("PyObject_AsCharBuffer");
  Python_PyObject_AsReadBuffer = resolve_optional("PyObject_AsReadBuffer");
  Python_PyObject_AsWriteBuffer = resolve_optional("PyObject_AsWriteBuffer");
  if (version_major >= 3) {
    Python__Py_FalseStruct = resolve("_Py_FalseStruct");
    Python_PyString_AsStringAndSize = resolve("PyBytes_AsStringAndSize");
  } else {
    Python__Py_FalseStruct = resolve("_Py_ZeroStruct");
    Python_PyString_AsStringAndSize = resolve("PyString_AsStringAndSize");
  }
  Python_PyLong_FromString = resolve("PyLong_FromString");
  Python_PyMem_Free = resolve("PyMem_Free");
  Python_PyThreadState_Get = resolve("PyThreadState_Get");
  Python_PyFrame_New = resolve("PyFrame_New");
  Python_PyCode_NewEmpty = resolve("PyCode_NewEmpty");
  if (version_major >= 3) {
    Python__Py_wfopen = resolve_optional("_Py_wfopen"); /* Python >=3.10 */
    Python__Py_fopen = resolve_optional("_Py_fopen");
  } else {
    Python2_PyCObject_FromVoidPtr = resolve("PyCObject_FromVoidPtr");
    Python2_PyCObject_AsVoidPtr = resolve("PyCObject_AsVoidPtr");
  }
  if (find_symbol(library, "PyUnicodeUCS2_AsEncodedString")) {
    ucs = UCS2;
  } else if (find_symbol(library, "PyUnicodeUCS4_AsEncodedString")) {
    ucs = UCS4;
  } else {
    ucs = UCS_NONE;
  }
#include "pyml_dlsyms.inc"
  Python_Py_Initialize();
  PyObject* sys = Python_PyImport_ImportModule("sys");
  if (!sys) {
    caml_failwith("cannot import module sys");
  }
  trace_refs_build = Python_PyObject_HasAttrString(sys, "getobjects");
  if (Is_block(debug_build_ocaml)) {
    debug_build = Int_val(Field(debug_build_ocaml, 0));
  } else {
    guess_debug_build();
  }
  tuple_empty = Python_PyTuple_New(0);
  caml_register_custom_operations(&pyops);
  CAMLreturn(Val_unit);
}

struct PyObjectDebug {
  PyObject* _ob_next;
  PyObject* _ob_prev;
  PyObjectDescr descr;
};

PyObjectDescr* pyobjectdescr(PyObject* obj) {
  if (trace_refs_build) {
    return &((struct PyObjectDebug*)obj)->descr;
  } else {
    return (PyObjectDescr*)obj;
  }
}

CAMLprim value py_is_debug_build() {
  CAMLparam0();
  CAMLreturn(Val_int(debug_build));
}

CAMLprim value py_finalize_library(value unit) {
  CAMLparam1(unit);
  pyml_assert_initialized();
  Py_DECREF(tuple_empty);
  if (library != get_default_library()) {
    close_library(library);
  }
  version_major = 0;
  ucs = UCS_NONE;
  CAMLreturn(Val_unit);
}

CAMLprim value py_unsetenv(value name_ocaml) {
  CAMLparam1(name_ocaml);
  const char* name = String_val(name_ocaml);
  if (unsetenv(name) == -1) {
    caml_failwith(strerror(errno));
  }
  CAMLreturn(Val_unit);
}

CAMLprim value py_get_UCS(value unit) {
  CAMLparam1(unit);
  pyml_assert_initialized();
  CAMLreturn(Val_int(ucs));
}

CAMLprim value PyNull_wrapper(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(CODE_NULL));
}

CAMLprim value PyNone_wrapper(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(CODE_NONE));
}

CAMLprim value PyTrue_wrapper(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(CODE_TRUE));
}

CAMLprim value PyFalse_wrapper(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(CODE_FALSE));
}

CAMLprim value PyTuple_Empty_wrapper(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(CODE_TUPLE_EMPTY));
}

enum pytype_labels {
  PyUnknown,
  Bool,
  Bytes,
  Callable,
  Capsule,
  Closure,
  Dict,
  Float,
  List,
  Int,
  Long,
  Module,
  NoneType,
  Null,
  Tuple,
  Type,
  Unicode,
  Iter,
  Set
};

CAMLprim value pytype(value object_ocaml) {
  CAMLparam1(object_ocaml);
  pyml_assert_initialized();
  PyObject* object = pyml_unwrap(object_ocaml);
  if (!object) {
    CAMLreturn(Val_int(Null));
  }
  PyObject* ob_type = pyobjectdescr(object)->ob_type;
  struct _typeobject* typeobj = (struct _typeobject*)pyobjectdescr(ob_type);
  unsigned long flags = typeobj->tp_flags;
  int result;
  if (ob_type == Python_PyBool_Type) {
    result = Bool;
  } else if (flags & Py_TPFLAGS_BYTES_SUBCLASS) {
    result = Bytes;
  } else if (Python_PyCallable_Check(object)) {
    result = Callable;
  } else if (Python27_PyCapsule_IsValid &&
             Python27_PyCapsule_IsValid(object, "ocaml-capsule")) {
    result = Capsule;
  } else if (Python27_PyCapsule_IsValid &&
             Python27_PyCapsule_IsValid(object, "ocaml-closure")) {
    result = Closure;
  } else if (flags & Py_TPFLAGS_DICT_SUBCLASS) {
    result = Dict;
  } else if (ob_type == Python_PyFloat_Type ||
             Python_PyType_IsSubtype(ob_type, Python_PyFloat_Type)) {
    result = Float;
  } else if (flags & Py_TPFLAGS_LIST_SUBCLASS) {
    result = List;
  } else if (flags & Py_TPFLAGS_INT_SUBCLASS) {
    result = Int;
  } else if (flags & Py_TPFLAGS_LONG_SUBCLASS) {
    result = Long;
  } else if (ob_type == Python_PyModule_Type ||
             Python_PyType_IsSubtype(ob_type, Python_PyModule_Type)) {
    result = Module;
  } else if (object == Python__Py_NoneStruct) {
    result = NoneType;
  } else if (flags & Py_TPFLAGS_TUPLE_SUBCLASS) {
    result = Tuple;
  } else if (flags & Py_TPFLAGS_TYPE_SUBCLASS) {
    result = Type;
  } else if (flags & Py_TPFLAGS_UNICODE_SUBCLASS) {
    result = Unicode;
  } else if (ob_type == Python_PySet_Type) {
    result = Set;
  } else if (typeobj->tp_iternext != NULL &&
             typeobj->tp_iternext != &Python27__PyObject_NextNotImplemented) {
    result = Iter;
  } else {
    result = PyUnknown;
  }
  CAMLreturn(Val_int(result));
}

CAMLprim value PyObject_CallFunctionObjArgs_wrapper(value callable_ocaml,
                                                    value arguments_ocaml) {
  CAMLparam2(callable_ocaml, arguments_ocaml);
  pyml_assert_initialized();
  PyObject* callable = pyml_unwrap(callable_ocaml);
  PyObject* result;
  mlsize_t argument_count = Wosize_val(arguments_ocaml);
  switch (argument_count) {
    case 0:
      result = Python_PyObject_CallFunctionObjArgs(callable, NULL);
      break;
    case 1:
      result = Python_PyObject_CallFunctionObjArgs(
          callable, pyml_unwrap(Field(arguments_ocaml, 0)), NULL);
      break;
    case 2:
      result = Python_PyObject_CallFunctionObjArgs(
          callable,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          NULL);
      break;
    case 3:
      result = Python_PyObject_CallFunctionObjArgs(
          callable,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          pyml_unwrap(Field(arguments_ocaml, 2)),
          NULL);
      break;
    case 4:
      result = Python_PyObject_CallFunctionObjArgs(
          callable,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          pyml_unwrap(Field(arguments_ocaml, 2)),
          pyml_unwrap(Field(arguments_ocaml, 3)),
          NULL);
      break;
    case 5:
      result = Python_PyObject_CallFunctionObjArgs(
          callable,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          pyml_unwrap(Field(arguments_ocaml, 2)),
          pyml_unwrap(Field(arguments_ocaml, 3)),
          pyml_unwrap(Field(arguments_ocaml, 4)),
          NULL);
      break;
    default:
      fprintf(stderr,
              "PyObject_CallFunctionObjArgs_wrapper not implemented for more "
              "than 5 arguments\n");
      exit(EXIT_FAILURE);
  }

  CAMLreturn(pyml_wrap(result, true));
}

CAMLprim value PyObject_CallMethodObjArgs_wrapper(value object_ocaml,
                                                  value name_ocaml,
                                                  value arguments_ocaml) {
  CAMLparam3(object_ocaml, name_ocaml, arguments_ocaml);
  pyml_assert_initialized();
  PyObject* object = pyml_unwrap(object_ocaml);
  PyObject* name = pyml_unwrap(name_ocaml);
  PyObject* result;
  mlsize_t argument_count = Wosize_val(arguments_ocaml);
  switch (argument_count) {
    case 0:
      result = Python_PyObject_CallMethodObjArgs(object, name, NULL);
      break;
    case 1:
      result = Python_PyObject_CallMethodObjArgs(
          object, name, pyml_unwrap(Field(arguments_ocaml, 0)), NULL);
      break;
    case 2:
      result = Python_PyObject_CallMethodObjArgs(
          object,
          name,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          NULL);
      break;
    case 3:
      result = Python_PyObject_CallMethodObjArgs(
          object,
          name,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          pyml_unwrap(Field(arguments_ocaml, 2)),
          NULL);
      break;
    case 4:
      result = Python_PyObject_CallMethodObjArgs(
          object,
          name,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          pyml_unwrap(Field(arguments_ocaml, 2)),
          pyml_unwrap(Field(arguments_ocaml, 3)),
          NULL);
      break;
    case 5:
      result = Python_PyObject_CallMethodObjArgs(
          object,
          name,
          pyml_unwrap(Field(arguments_ocaml, 0)),
          pyml_unwrap(Field(arguments_ocaml, 1)),
          pyml_unwrap(Field(arguments_ocaml, 2)),
          pyml_unwrap(Field(arguments_ocaml, 3)),
          pyml_unwrap(Field(arguments_ocaml, 4)),
          NULL);
      break;
    default:
      fprintf(stderr,
              "PyObject_CallMethodObjArgs_wrapper not implemented for more "
              "than 5 arguments\n");
      exit(EXIT_FAILURE);
  }

  CAMLreturn(pyml_wrap(result, true));
}

CAMLprim value pyml_capsule_check(value v) {
  CAMLparam1(v);
  pyml_assert_initialized();
  PyObject* o = pyml_unwrap(v);
  PyObject* ob_type = pyobjectdescr(o)->ob_type;
  int check_result = ob_type == Python_PyCapsule_Type;
  CAMLreturn(Val_int(check_result));
}

CAMLprim value pyml_wrap_value(value v) {
  CAMLparam1(v);
  pyml_assert_initialized();
  PyObject* result = camlwrap_capsule(v, NULL, 0);
  CAMLreturn(pyml_wrap(result, true));
}

CAMLprim value pyml_unwrap_value(value x_ocaml) {
  CAMLparam1(x_ocaml);
  CAMLlocal1(v);
  pyml_assert_initialized();
  PyObject* x = pyml_unwrap(x_ocaml);
  void* p = unwrap_capsule(x, "ocaml-capsule");
  if (!p) {
    fprintf(stderr, "pyml_unwrap_value: type mismatch");
    exit(EXIT_FAILURE);
  }
  v = *(value*)p;
  CAMLreturn(v);
}

CAMLprim value PyErr_Fetch_wrapper(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  pyml_assert_initialized();
  PyObject *excType, *excValue, *excTraceback;
  Python_PyErr_Fetch(&excType, &excValue, &excTraceback);
  Python_PyErr_NormalizeException(&excType, &excValue, &excTraceback);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, pyml_wrap(excType, false));
  Store_field(result, 1, pyml_wrap(excValue, false));
  Store_field(result, 2, pyml_wrap(excTraceback, false));
  CAMLreturn(result);
}

// PyErr_Restore steals the references.
// https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Restore
// However the objects can be null, so we do not want to run Py_INCREF if
// this is the case as this would trigger some segfaults.
CAMLprim value PyErr_Restore_wrapper(value arg0_ocaml,
                                     value arg1_ocaml,
                                     value arg2_ocaml) {
  CAMLparam3(arg0_ocaml, arg1_ocaml, arg2_ocaml);
  pyml_assert_initialized();
  PyObject* arg0 = pyml_unwrap(arg0_ocaml);
  if (arg0)
    Py_INCREF(arg0);
  PyObject* arg1 = pyml_unwrap(arg1_ocaml);
  if (arg1)
    Py_INCREF(arg1);
  PyObject* arg2 = pyml_unwrap(arg2_ocaml);
  if (arg2)
    Py_INCREF(arg2);
  Python_PyErr_Restore(arg0, arg1, arg2);
  CAMLreturn(Val_unit);
}

CAMLprim value pyml_wrap_string_option(const char* s) {
  CAMLparam0();
  CAMLlocal1(result);
  if (!s) {
    CAMLreturn(Val_int(0));
  }
  result = caml_alloc_tuple(1);
  Store_field(result, 0, caml_copy_string(s));
  CAMLreturn(result);
}

CAMLprim value pyrefcount(value pyobj) {
  CAMLparam1(pyobj);
  PyObject* obj = pyml_unwrap(pyobj);
  CAMLreturn(Val_int(pyobjectdescr(obj)->ob_refcnt));
}

static value pyml_wrap_wide_string(wchar_t* ws) {
  CAMLparam0();
  CAMLlocal1(result);
  size_t n = wcstombs(NULL, ws, 0);
  if (n == (size_t)-1) {
    fprintf(stderr, "pyml_wrap_wide_string failure.\n");
    exit(EXIT_FAILURE);
  }
  char* s = xmalloc((n + 1) * sizeof(char));
  wcstombs(s, ws, n);
  result = caml_copy_string(s);
  free(s);
  CAMLreturn(result);
}

static wchar_t* wide_string_of_string(const char* s) {
  size_t n = mbstowcs(NULL, s, 0);
  if (n == (size_t)-1) {
    fprintf(stderr, "wide_string_of_string failure.\n");
    exit(EXIT_FAILURE);
  }
  wchar_t* ws = xmalloc((n + 1) * sizeof(wchar_t));
  mbstowcs(ws, s, n + 1);
  return ws;
}

static wchar_t* pyml_unwrap_wide_string(value string_ocaml) {
  CAMLparam1(string_ocaml);
  wchar_t* ws = wide_string_of_string(String_val(string_ocaml));
  CAMLreturnT(wchar_t*, ws);
}

static int16_t* pyml_unwrap_ucs2(value array_ocaml) {
  CAMLparam1(array_ocaml);
  mlsize_t len = Wosize_val(array_ocaml);
  int16_t* result = xmalloc(len * sizeof(int16_t));
  size_t i;
  for (i = 0; i < len; i++) {
    result[i] = Field(array_ocaml, i);
  }
  CAMLreturnT(int16_t*, result);
}

static int32_t* pyml_unwrap_ucs4(value array_ocaml) {
  CAMLparam1(array_ocaml);
  mlsize_t len = Wosize_val(array_ocaml);
  int32_t* result = xmalloc(len * sizeof(int32_t));
  size_t i;
  for (i = 0; i < len; i++) {
    result[i] = Field(array_ocaml, i);
  }
  CAMLreturnT(int32_t*, result);
}

static value pyml_wrap_ucs2_option(int16_t* buffer) {
  CAMLparam0();
  CAMLlocal2(result, array);
  mlsize_t len;
  if (buffer == NULL) {
    CAMLreturn(Val_int(0));
  }
  len = 0;
  while (buffer[len]) {
    len++;
  }
  array = caml_alloc_tuple(len);
  size_t i;
  for (i = 0; i < len; i++) {
    Store_field(array, i, buffer[i]);
  }
  result = caml_alloc_tuple(1);
  Store_field(result, 0, array);
  CAMLreturn(result);
}

static value pyml_wrap_ucs4_option_and_free(int32_t* buffer, bool free) {
  CAMLparam0();
  CAMLlocal2(result, array);
  mlsize_t len;
  if (buffer == NULL) {
    CAMLreturn(Val_int(0));
  }
  len = 0;
  while (buffer[len]) {
    len++;
  }
  array = caml_alloc_tuple(len);
  size_t i;
  for (i = 0; i < len; i++) {
    Store_field(array, i, buffer[i]);
  }
  result = caml_alloc_tuple(1);
  Store_field(result, 0, array);
  if (free) {
    Python_PyMem_Free(buffer);
  }
  CAMLreturn(result);
}

#define StringAndSize_wrapper(func, byte_type)              \
  CAMLprim value func##_wrapper(value arg_ocaml) {          \
    CAMLparam1(arg_ocaml);                                  \
    CAMLlocal2(result, string);                             \
    PyObject* arg = pyml_unwrap(arg_ocaml);                 \
    byte_type* buffer;                                      \
    Py_ssize_t length;                                      \
    int return_value;                                       \
    return_value = Python_##func(arg, &buffer, &length);    \
    if (return_value == -1) {                               \
      CAMLreturn(Val_int(0));                               \
    }                                                       \
    string = caml_alloc_initialized_string(length, buffer); \
    result = caml_alloc_tuple(1);                           \
    Store_field(result, 0, string);                         \
    CAMLreturn(result);                                     \
  }

StringAndSize_wrapper(PyString_AsStringAndSize, char);
StringAndSize_wrapper(PyObject_AsCharBuffer, const char);
StringAndSize_wrapper(PyObject_AsReadBuffer, const void);
StringAndSize_wrapper(PyObject_AsWriteBuffer, void);

static FILE* open_file(value file, const char* mode) {
  CAMLparam1(file);
  FILE* result;
  if (Tag_val(file) == 0) {
    const char* filename = String_val(Field(file, 0));
    if (Python__Py_fopen != NULL) {
      result = Python__Py_fopen(filename, mode);
    } else if (Python__Py_wfopen != NULL) {
      wchar_t* wide_filename = wide_string_of_string(filename);
      wchar_t* wide_mode = wide_string_of_string(mode);
      result = Python__Py_wfopen(wide_filename, wide_mode);
      free(wide_mode);
      free(wide_filename);
    } else {
      result = fopen(filename, mode);
    }
  } else {
    result = file_of_file_descr(Field(file, 0), mode);
  }
  CAMLreturnT(FILE*, result);
}

static void close_file(value file, FILE* file_struct) {
  CAMLparam1(file);
  fclose(file_struct);
  CAMLreturn0;
}

/* Numpy */

void** pyml_get_pyarray_api(PyObject* c_api) {
  if (version_major >= 3) {
    return (void**)Python27_PyCapsule_GetPointer(c_api, NULL);
  } else {
    return (void**)Python2_PyCObject_AsVoidPtr(c_api);
  }
}

CAMLprim value get_pyarray_type(value numpy_api_ocaml) {
  CAMLparam1(numpy_api_ocaml);
  PyObject* c_api = pyml_unwrap(numpy_api_ocaml);
  void** PyArray_API = pyml_get_pyarray_api(c_api);
  PyObject* result = PyArray_API[2];
  CAMLreturn(pyml_wrap(result, false));
}

CAMLprim value pyarray_of_floatarray_wrapper(value numpy_api_ocaml,
                                             value array_type_ocaml,
                                             value array_ocaml) {
  CAMLparam3(numpy_api_ocaml, array_type_ocaml, array_ocaml);
  pyml_assert_initialized();
  PyObject* c_api = pyml_unwrap(numpy_api_ocaml);
  void** PyArray_API = pyml_get_pyarray_api(c_api);
  PyObject* (*PyArray_New)(PyTypeObject*,
                           int,
                           npy_intp*,
                           int,
                           npy_intp*,
                           void*,
                           int,
                           int,
                           PyObject*) = PyArray_API[93];
  npy_intp length = Wosize_val(array_ocaml);
#ifndef ARCH_SIXTYFOUR
  length /= 2;
#endif
  void* data = (double*)array_ocaml;
  PyTypeObject(*PyArray_SubType) = (PyTypeObject*)pyml_unwrap(array_type_ocaml);
  PyObject* result = PyArray_New(PyArray_SubType,
                                 1,
                                 &length,
                                 NPY_DOUBLE,
                                 NULL,
                                 data,
                                 0,
                                 NPY_ARRAY_CARRAY,
                                 NULL);
  CAMLreturn(pyml_wrap(result, true));
}

CAMLprim value pyarray_move_floatarray_wrapper(value numpy_array_ocaml,
                                               value array_ocaml) {
  CAMLparam2(numpy_array_ocaml, array_ocaml);
  pyml_assert_initialized();
  PyObject* numpy_array = pyml_unwrap(numpy_array_ocaml);
  PyArrayObject_fields* fields =
      (PyArrayObject_fields*)pyobjectdescr(numpy_array);
  fields->data = (void*)array_ocaml;
  CAMLreturn(Val_unit);
}

CAMLprim value PyLong_FromString_wrapper(value str_ocaml, value base_ocaml) {
  CAMLparam2(str_ocaml, base_ocaml);
  CAMLlocal1(result);
  pyml_assert_initialized();
  const char* str = String_val(str_ocaml);
  const char* pend;
  int base = Int_val(base_ocaml);
  PyObject* l = Python_PyLong_FromString(str, &pend, base);
  ssize_t len = pend - str;
  result = caml_alloc_tuple(2);
  Store_field(result, 0, pyml_wrap(l, true));
  Store_field(result, 1, Val_int(len));
  CAMLreturn(result);
}

CAMLprim value Python27_PyCapsule_IsValid_wrapper(value arg0_ocaml,
                                                  value arg1_ocaml) {
  CAMLparam2(arg0_ocaml, arg1_ocaml);

  pyml_assert_initialized();
  if (!Python27_PyCapsule_IsValid) {
    caml_failwith("PyCapsule_IsValid is only available in Python >2.7");
  }
  PyObject* arg0 = pyml_unwrap(arg0_ocaml);
  const char* arg1 = String_val(arg1_ocaml);
  int result = Python27_PyCapsule_IsValid(arg0, arg1);
  CAMLreturn(Val_int(result));
}

CAMLprim value pyml_pyframe_new(value filename_ocaml,
                                value funcname_ocaml,
                                value lineno_ocaml) {
  CAMLparam3(filename_ocaml, funcname_ocaml, lineno_ocaml);
  const char* filename = String_val(filename_ocaml);
  const char* funcname = String_val(funcname_ocaml);
  int lineno = Int_val(lineno_ocaml);
  PyObject* code = Python_PyCode_NewEmpty(filename, funcname, lineno);
  PyObject* globals = Python_PyDict_New();
  PyObject* result =
      Python_PyFrame_New(Python_PyThreadState_Get(), code, globals, NULL);
  Py_DECREF(code);
  Py_DECREF(globals);
  CAMLreturn(pyml_wrap(result, true));
}

#include "pyml_wrappers.inc"
