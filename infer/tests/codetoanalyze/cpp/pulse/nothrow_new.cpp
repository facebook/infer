/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstddef>
#include <cstdlib>
#include <exception>
#include <memory>
#include <new>

namespace nothrow_new {

struct Pod {
  int count;
  int* data;
};

struct WithCtor {
  int x;
  WithCtor() : x(0) {}
  explicit WithCtor(int v) : x(v) {}
  ~WithCtor() {}
};

int pod_unchecked_bad() {
  Pod* p = new (std::nothrow) Pod{};
  p->count = 1;
  int c = p->count;
  delete p;
  return c;
}

int pod_checked_ok() {
  Pod* p = new (std::nothrow) Pod{};
  if (p == nullptr) {
    return -1;
  }
  p->count = 1;
  int c = p->count;
  delete p;
  return c;
}

int scalar_init_unchecked_bad() {
  int* p = new (std::nothrow) int(5);
  int r = *p;
  delete p;
  return r;
}

int scalar_init_checked_ok() {
  int* p = new (std::nothrow) int(5);
  if (!p) {
    return -1;
  }
  int r = *p;
  delete p;
  return r;
}

int checked_then_dereferenced_bad() {
  int* p = new (std::nothrow) int(5);
  if (p == nullptr) {
    // missing early return
  }
  int r = *p;
  delete p;
  return r;
}

int array_unchecked_bad(std::size_t n) {
  int* a = new (std::nothrow) int[n];
  a[0] = 1;
  int c = a[0];
  delete[] a;
  return c;
}

int array_checked_ok(std::size_t n) {
  int* a = new (std::nothrow) int[n];
  if (!a) {
    return -1;
  }
  a[0] = 1;
  int c = a[0];
  delete[] a;
  return c;
}

int array_init_list_checked_ok() {
  int* a = new (std::nothrow) int[3]{1, 2, 3};
  if (a == nullptr) {
    return -1;
  }
  int c = a[2];
  delete[] a;
  return c;
}

int ctor_unchecked_bad() {
  WithCtor* p = new (std::nothrow) WithCtor(3);
  int r = p->x;
  delete p;
  return r;
}

int ctor_checked_ok() {
  WithCtor* p = new (std::nothrow) WithCtor(3);
  if (p == nullptr) {
    return 0;
  }
  int r = p->x;
  delete p;
  return r;
}

int ctor_array_checked_ok() {
  WithCtor* a = new (std::nothrow) WithCtor[2];
  if (a == nullptr) {
    return 0;
  }
  int r = a[1].x;
  delete[] a;
  return r;
}

void leak_bad() {
  int* p = new (std::nothrow) int;
  if (p) {
    *p = 1;
  }
}

struct ThrowingNothrowNew {
  int x;
  ThrowingNothrowNew() : x(0) {}
  static void* operator new(std::size_t size, const std::nothrow_t&);
};

// an allocation function that is not noexcept never returns null
int throwing_nothrow_operator_new_ok() {
  ThrowingNothrowNew* p = new (std::nothrow) ThrowingNothrowNew();
  return p->x;
}

WithCtor* make_with_ctor() { return new (std::nothrow) WithCtor(1); }

int interproc_unchecked_bad() {
  WithCtor* p = make_with_ctor();
  int r = p->x;
  delete p;
  return r;
}

int unique_ptr_checked_ok() {
  std::unique_ptr<WithCtor> p(new (std::nothrow) WithCtor(1));
  if (!p) {
    return 0;
  }
  return p->x;
}

struct Resource {
  int* p;
  Resource() : p(new int(0)) {}
  ~Resource() { delete p; }
};

struct Holder {
  int v;
  explicit Holder(const Resource& r) : v(*r.p) {}
};

// the temporary is only constructed, and destroyed, when the allocation
// succeeds
int temporary_in_initializer_checked_ok() {
  Holder* h = new (std::nothrow) Holder(Resource());
  if (h == nullptr) {
    return 0;
  }
  int v = h->v;
  delete h;
  return v;
}

void fail_with_abort() { abort(); }

void fail_with_terminate() { std::terminate(); }

void fail_analyzer_noreturn() __attribute__((analyzer_noreturn));

struct FatalOnDestruction {
  ~FatalOnDestruction() { abort(); }
};

// The checks below call functions that never return without being declared
// noreturn. They are still reported with the default --pulse-force-continue.
int abort_helper_checked_ok() {
  WithCtor* p = new (std::nothrow) WithCtor(1);
  if (p == nullptr) {
    fail_with_abort();
  }
  int r = p->x;
  delete p;
  return r;
}

int terminate_helper_checked_ok() {
  WithCtor* p = new (std::nothrow) WithCtor(1);
  if (p == nullptr) {
    fail_with_terminate();
  }
  int r = p->x;
  delete p;
  return r;
}

int fatal_temporary_checked_ok() {
  WithCtor* p = new (std::nothrow) WithCtor(1);
  if (p == nullptr) {
    FatalOnDestruction();
  }
  int r = p->x;
  delete p;
  return r;
}

// analyzer_noreturn is ignored
int FP_analyzer_noreturn_helper_checked_ok() {
  WithCtor* p = new (std::nothrow) WithCtor(1);
  if (p == nullptr) {
    fail_analyzer_noreturn();
  }
  int r = p->x;
  delete p;
  return r;
}

struct AbortingNothrowNew {
  int x;
  AbortingNothrowNew() : x(0) {}
  static void* operator new(std::size_t size, const std::nothrow_t&) noexcept {
    void* p = malloc(size);
    if (p == nullptr) {
      abort();
    }
    return p;
  }
  static void operator delete(void* p) { free(p); }
};

// the allocation function that the new-expression selects is not analyzed, so
// it is assumed to return null on failure as the standard one does
int FP_class_nothrow_operator_new_aborts_ok() {
  AbortingNothrowNew* p = new (std::nothrow) AbortingNothrowNew();
  int r = p->x;
  delete p;
  return r;
}

// calls to the allocation function outside of a new-expression are not modelled
int FN_operator_new_call_unchecked_bad() {
  int* p = static_cast<int*>(::operator new(sizeof(int), std::nothrow));
  *p = 1;
  int r = *p;
  ::operator delete(p);
  return r;
}

} // namespace nothrow_new
