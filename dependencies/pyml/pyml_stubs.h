#ifndef _PYML_STUBS_H_
#define _PYML_STUBS_H_
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>

/* The following definitions are extracted and simplified from
#include <Python.h>
*/

/* ssize_t is POSIX and not defined with Visual Studio */
/* See for instance https://github.com/vlm/asn1c/issues/159 */
#if defined(_MSC_VER)
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
#else
#include <unistd.h>
#endif

typedef ssize_t Py_ssize_t;

#define _PyObject_HEAD_EXTRA

#define PyObject_HEAD                   \
    _PyObject_HEAD_EXTRA                \
    Py_ssize_t ob_refcnt;               \
    PyObject *ob_type;

typedef void PyObject;

typedef struct {
    Py_ssize_t ob_refcnt;
    PyObject *ob_type;
} PyObjectDescr;

PyObjectDescr *pyobjectdescr(PyObject *obj);

typedef struct {
    PyObjectDescr ob_base;
    Py_ssize_t ob_size;
} PyVarObject;

typedef void (*destructor)(PyObject *);

typedef struct _typeobject {
    PyVarObject ob_base;
    const char *tp_name;
    Py_ssize_t tp_basicsize, tp_itemsize;
    destructor tp_dealloc;
    void *tp_print;
    void *tp_getattr;
    void *tp_setattr;
    void *tp_as_async;
    void *tp_repr;
    void *tp_as_number;
    void *tp_as_sequence;
    void *tp_as_mapping;
    void *tp_hash;
    void *tp_call;
    void *tp_str;
    void *tp_getattro;
    void *tp_setattro;
    void *tp_as_buffer;
    unsigned long tp_flags;
    const char *tp_doc;
    void *tp_traverse;
    void *tp_clear;
    void *tp_richcompare;
    Py_ssize_t tp_weaklistoffset;
    void *tp_iter;
    void *tp_iternext;
    void *tp_methods;
    void *tp_members;
    void *tp_getset;
    void *tp_base;
    PyObject *tp_dict;
    void *tp_descr_get;
    void *tp_descr_set;
    Py_ssize_t tp_dictoffset;
    void *tp_init;
    void *tp_alloc;
    void *tp_new;
    void *tp_free;
    void *tp_is_gc;
    PyObject *tp_bases;
    PyObject *tp_mro;
    PyObject *tp_cache;
    PyObject *tp_subclasses;
    PyObject *tp_weaklist;
    void *tp_del;
    unsigned int tp_version_tag;
    void *tp_finalize;
/* #ifdef COUNT_ALLOCS */
    Py_ssize_t tp_allocs;
    Py_ssize_t tp_frees;
    Py_ssize_t tp_maxalloc;
    struct _typeobject *tp_prev;
    struct _typeobject *tp_next;
/* #endif */
} PyTypeObject;

void
pyml_assert_initialized();

void
pyml_assert_python2();

void
pyml_assert_ucs2();

void
pyml_assert_ucs4();

void
pyml_assert_python3();

/* Numpy */

/* from ndarraytypes.h */

enum NPY_TYPES {
    NPY_BOOL=0,
                    NPY_BYTE, NPY_UBYTE,
                    NPY_SHORT, NPY_USHORT,
                    NPY_INT, NPY_UINT,
                    NPY_LONG, NPY_ULONG,
                    NPY_LONGLONG, NPY_ULONGLONG,
                    NPY_FLOAT, NPY_DOUBLE, NPY_LONGDOUBLE,
                    NPY_CFLOAT, NPY_CDOUBLE, NPY_CLONGDOUBLE,
                    NPY_OBJECT=17,
                    NPY_STRING, NPY_UNICODE,
                    NPY_VOID,
                    /*
                     * New 1.6 types appended, may be integrated
                     * into the above in 2.0.
                     */
                    NPY_DATETIME, NPY_TIMEDELTA, NPY_HALF,

                    NPY_NTYPES,
                    NPY_NOTYPE,
                    NPY_CHAR,      /* special flag */
                    NPY_USERDEF=256,  /* leave room for characters */

                    /* The number of types not including the new 1.6 types */
                    NPY_NTYPES_ABI_COMPATIBLE=21
};

#define NPY_ARRAY_C_CONTIGUOUS    0x0001
#define NPY_ARRAY_F_CONTIGUOUS    0x0002
#define NPY_ARRAY_OWNDATA         0x0004
#define NPY_ARRAY_ALIGNED         0x0100
#define NPY_ARRAY_WRITEABLE       0x0400

#define NPY_ARRAY_BEHAVED      (NPY_ARRAY_ALIGNED | \
                                NPY_ARRAY_WRITEABLE)
#define NPY_ARRAY_CARRAY       (NPY_ARRAY_C_CONTIGUOUS | \
                                NPY_ARRAY_BEHAVED)
#define NPY_ARRAY_FARRAY       (NPY_ARRAY_F_CONTIGUOUS | \
                                NPY_ARRAY_BEHAVED)

/* From pyport.h */
typedef intptr_t        Py_intptr_t;

/* From npy_common.h */
typedef Py_intptr_t npy_intp;

void **
pyml_get_pyarray_api(PyObject *c_api);

#define Py_INCREF(op)                                            \
    ((pyobjectdescr(op))->ob_refcnt++)

#define Py_XINCREF(op)                                           \
    do {                                                         \
        PyObjectDescr *_py_xincref_tmp =                         \
            pyobjectdescr((PyObject *)(op));                     \
        if (_py_xincref_tmp != NULL)                             \
            Py_INCREF(_py_xincref_tmp);                          \
    } while (0)

#define Py_DECREF(op)                                            \
    do {                                                         \
        PyObjectDescr *_py_decref_tmp =                          \
            pyobjectdescr((PyObject *)(op));                     \
        if (--(_py_decref_tmp)->ob_refcnt == 0)                  \
            ((struct _typeobject *)                              \
             pyobjectdescr(_py_decref_tmp->ob_type))             \
                ->tp_dealloc(op);                                \
    } while (0)

/* from ndarraytypes.h */

typedef struct _PyArray_Descr {
        PyObject_HEAD
        PyTypeObject *typeobj;
        char kind;
        char type;
        char byteorder;
        char flags;
        int type_num;
        int elsize;
        int alignment;
        struct _arr_descr *subarray;
        PyObject *fields;
        PyObject *names;
        void *f;
        PyObject *metadata;
        void *c_metadata;
        int hash;
} PyArray_Descr;

typedef struct _arr_descr {
        PyArray_Descr *base;
        PyObject *shape;
} PyArray_ArrayDescr;

typedef struct tagPyArrayObject_fields {
    PyObject_HEAD
    char *data;
    int nd;
    npy_intp *dimensions;
    npy_intp *strides;
    PyObject *base;
    PyArray_Descr *descr;
    int flags;
    PyObject *weakreflist;
} PyArrayObject_fields;

typedef struct {
    int cf_flags;
    int cf_feature_version; /* Python >=3.8 */
} PyCompilerFlags;

#define Py_TPFLAGS_INT_SUBCLASS         (1L<<23)
#define Py_TPFLAGS_LONG_SUBCLASS        (1UL << 24)
#define Py_TPFLAGS_LIST_SUBCLASS        (1UL << 25)
#define Py_TPFLAGS_TUPLE_SUBCLASS       (1UL << 26)
#define Py_TPFLAGS_BYTES_SUBCLASS       (1UL << 27)
#define Py_TPFLAGS_UNICODE_SUBCLASS     (1UL << 28)
#define Py_TPFLAGS_DICT_SUBCLASS        (1UL << 29)
#define Py_TPFLAGS_BASE_EXC_SUBCLASS    (1UL << 30)
#define Py_TPFLAGS_TYPE_SUBCLASS        (1UL << 31)

#define Py_LT 0
#define Py_LE 1
#define Py_EQ 2
#define Py_NE 3
#define Py_GT 4
#define Py_GE 5

typedef PyObject *(*PyCFunction)(PyObject *, PyObject *);

typedef struct PyMethodDef {
    const char *ml_name;
    PyCFunction ml_meth;
    int ml_flags;
    const char	*ml_doc;
} PyMethodDef;

typedef void (*PyCapsule_Destructor)(PyObject *);

#endif /* _PYML_STUBS_H_ */
