/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <atomic>
#include <cstdlib>
#include <memory>
#include <string>
#include <type_traits>

void assign_zero_ok() {
  int x[2];
  x[1] = 42;
}

void deref_nullptr_bad() {
  int* p = nullptr;
  *p = 42;
}

void guarded_nullptr_ok() {
  int* p = nullptr;
  if (p != nullptr) {
    *p = 42;
  }
}

struct X {
  void foo();
};

bool choice();

X* may_return_nullptr() {
  if (choice()) {
    return nullptr;
  }
  return new X();
}

void no_check_return_bad() {
  X* x = may_return_nullptr();
  x->foo();
}

void check_return_ok() {
  X* x = may_return_nullptr();
  if (x != nullptr) {
    x->foo();
  }
}

void compare_to_null(void* x) {
  if (x) {
  }
}

void deref_after_compare_ok(int* x) {
  compare_to_null(x);
  *x = 42;
}

bool return_true() { return std::true_type{}; }

void std_true_type_impossible_deref_ok() {
  int* x = nullptr;
  if (!return_true()) {
    *x = 42;
  }
}

void std_true_type_deref_bad() {
  int* x = nullptr;
  if (return_true()) {
    *x = 42;
  }
}

bool return_false() { return std::false_type{}; }

void std_false_type_impossible_deref_ok() {
  int* x = nullptr;
  if (return_false()) {
    *x = 42;
  }
}

void std_false_type_deref_bad() {
  int* x = nullptr;
  if (!return_false()) {
    *x = 42;
  }
}

std::atomic<bool> global_var{true};

namespace ns1 {
namespace ns2 {
void fun_abort(bool b) {
  bool abort = true;
  if (b) {
    abort = global_var.load();
  } else {
    abort = true;
  }
  if (abort) {
    std::abort();
  }
}
} // namespace ns2
} // namespace ns1

X* getX(bool b) {
  if (b) {
    return new X();
  } else {
    ns1::ns2::fun_abort(true);
  }

  return nullptr;
}

void call_modeled_abort_ok() { getX(false)->foo(); }

struct S {
  int field;
};

void set_S();

struct T {
  static S*& get() {
    auto& s = T::getRaw();
    if (T::getRaw() == nullptr) {
      set_S();
    }
    return s;
  }

  static S*& getRaw() {
    thread_local S* s = nullptr;
    return s;
  }
};

void set_S() {
  auto& s = T::getRaw();
  if (s != nullptr) {
    return;
  }

  s = (S*)calloc(1, sizeof(S));
}

int thread_local_was_set_ok() { return T::get()->field; }

struct Item {
  X* get() const;
};

struct Handle {
  X* get() const noexcept {
    return item_.get() == nullptr ? nullptr : toX(item_);
  }

  X* operator->() const noexcept {
    // dynamic check get() != null
    return get();
  }

 private:
  Item item_{};
  static X* toX(Item item);
};

// We do not want to report nullptr dereference in this case
// as we "know" that Item::get does not return null, however
// at the moment we are not able to show it in pulse.
// That's why as a workaround we model the analysis of Handle::get`
// to return non-null
void explicit_check_for_null_ok(Handle h) { return h->foo(); }

X* checks_for_null() { return getX(true) == nullptr ? nullptr : new X(); }

void cannot_be_null_ok() { return checks_for_null()->foo(); }

void free_nullptr_ok() {
  int* p = nullptr;
  free(p);
}

void delete_nullptr_ok() {
  int* p = nullptr;
  delete p;
}

void FN_test_after_dereference_latent(int* x) {
  // create a path split where x==0 in one of the paths
  if (x == 0)
    ;
  *x = 42;
}

void call_test_after_dereference_bad() {
  FN_test_after_dereference_latent(NULL);
}

void test_after_dereference2_latent(int* x) {
  *x = 42;
  if (x == 0)
    ;
}

void call_test_after_dereference2_bad() {
  test_after_dereference2_latent(NULL);
}

enum Type { STRING };

static constexpr Type typeGlobal = STRING;

struct D {
  Type type;
  std::string string;

  D(std::string s) : type(STRING) { new (&string) std::string(std::move(s)); }

  std::string const* get() const& {
    if (type != typeGlobal) {
      return nullptr;
    }
    return &string;
  }

  std::string to(const std::string& value) const { return value.c_str(); };

  std::string asString() const {
    if (type == STRING) {
      const std::string& value = *get();
      return to(value);
    }
    return "";
  }
};

std::string global_const_skipped_function_ok() {
  D* s = new D("");
  std::shared_ptr<D> ptr(s);
  return ptr->asString();
}

struct SomeClass {
  X* pointer_;
  int field_init_to_zero_{0};
  explicit SomeClass(X* ptr) : pointer_(ptr) {
    if (pointer_) {
    }
  }
};

class SomeDerivedClass : public SomeClass {
 public:
  explicit SomeDerivedClass(X* ptr) : SomeClass(ptr) { ptr->foo(); }
};

X* unknown_function_X();

// this creates a null derefer false positives with a non-sensical "forked"
// trace (one sub-trace for where 0 came from, which in this case ends in the
// unrelated "field_init_to_zero" assignment, and one sub-trace for the
// dereference in ptr->foo() in the constructor), which should be ignored
void createSomeDerivedClass_from_unknown_function_ok() {
  SomeDerivedClass something(unknown_function_X());
}
