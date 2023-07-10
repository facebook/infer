/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <iostream>
#include <memory>
#include <string>

namespace use_after_destructor {

struct S {
  int* f;
  S(int i) {
    f = new int;
    *f = i;
  }

  // missing: operator= to copy the pointer. double delete can happen if
  // operator= is called

  ~S() { delete f; }

  void reinit(S s) {
    f = new int;
    *f = *(s.f);
  }
};

// destructor called at end of function, no issues
void normal_scope_destructor_ok() { S s(1); }

void nested_scope_destructor_ok() {
  { S s(1); }
}

int reinit_after_explicit_destructor_ok() {
  S s(1);
  s.~S();
  s.reinit(S(2));
  return *s.f;
}

int reinit_after_explicit_destructor_bad() {
  S s(1);
  s.~S();
  s = S(2); // a temporary is created then operator= is called
  return *s.f;
}

int reinit_after_explicit_destructor2_bad() {
  S s(1);
  S s2(2);
  s.~S();
  s = s2; // operator=
  return *s.f;
  // [s2.f] is [s.f] so gets deleted twice
}

void placement_new_explicit_destructor_ok() {
  char buf[sizeof(S)];
  {
    S* s = new (buf) S(1);
    s->~S();
  }
  {
    // this use of [buf] shouldn't be flagged
    S* s = new (buf) S(2);
    s->~S();
  }
}

void double_destructor_bad() {
  S s(1);
  s.~S();
  // destructor will be called again after S goes out of scope, which is
  // undefined behavior
}

int use_after_destructor_bad() {
  S s(1);
  s.~S();
  int ret = *s.f;
  s = S{2};
  return ret;
}

// S::operator= doesn't copy resources correctly
void use_after_scope1_bad() {
  S s(1);
  {
    S tmp(2);
    s = tmp; // translated as operator=(s, tmp)
  } // destructor for tmp runs here
  // destructor for s here; second time the value held by s has been destructed
}

// same as above
void use_after_scope2_bad() {
  S s(1);
  { s = S(1); }
}

struct POD {
  int f;
};

// this code is ok since double-destructing POD struct is ok
void destruct_twice_ok() {
  POD p{1};
  {
    POD tmp{2};
    p = tmp;
  } // destructor for tmp
} // destructor for p runs here, but it's harmless

class Subclass : virtual POD {
  int* f;
  Subclass() { f = new int; }

  /** frontend code for this destructor will be:
   * ~Subclass:
   *  __infer_inner_destructor_~Subclass(this)
   *  __infer_inner_destructor_~POD(this)
   *
   * __infer_inner_destructor_~Subclass:
   *  delete f;
   *
   * We need to be careful not to warn that this has been double-destructed
   */
  ~Subclass() { delete f; }
};

void basic_placement_new_ok() {
  S* ptr = new S(1);
  ptr->~S();
  S* tptr = new (ptr) S(1);
  delete tptr;
}

// the correct code is shown in basic_placement_new_ok()
void placement_new_wrong_order_bad() {
  S* ptr = new S(1);
  S* tptr = new (ptr) S(1); // leaking ptr->f
  tptr->~S();
  delete ptr; // deleting tptr->f again because of the aliasing induced by
              // placement new
}

int* destruct_pointer_contents_then_placement_new1_ok(S* s) {
  s->~S();
  new (s) S(1);
  return s->f;
}

int* destruct_pointer_contents_then_placement_new2_ok(S* s) {
  s->~S();
  new (&(s->f)) S(1);
  return s->f;
}

int* placement_new_aliasing1_bad() {
  S* s = new S(1);
  s->~S();
  auto alias = new (s) S(2);
  delete alias; // this deletes s too
  return s->f; // bad, accessing freed memory
}

int* placement_new_aliasing2_bad() {
  S* s = new S(1);
  s->~S();
  auto alias = new (s) S(2);
  delete s; // this deletes alias too
  return alias->f; // bad, accessing freed memory
}

int* placement_new_aliasing3_bad() {
  S* s = new S(1);
  s->~S();
  S* alias = s;
  auto alias_placement = new (s) S(2);
  delete s; // this deletes alias too
  return alias->f; // bad, accessing freed memory
}

void placement_new_non_var_ok() {
  struct M {
    S* s;
  } m;
  m.s = new S(1);
  m.s->~S();
  new (m.s) S(2);
  delete m.s;
}

S* return_placement_new_ok() {
  auto mem = new S(1);
  mem->~S();
  return new (mem) S(2);
}

void destructor_in_loop_ok() {
  for (int i = 0; i < 10; i++) {
    S s(1);
  }
}

int FN_use_after_scope3_bad() {
  int* p;
  {
    int value = 3;
    p = &value;
  } // we do not know in the plugin that value is out of scope
  return *p;
}

struct C {
  C(int v) : f(v){};
  ~C();
  int f;
};

int use_after_scope4_bad() {
  C* pc;
  {
    C c(3);
    pc = &c;
  }
  return pc->f;
}

struct B {
  ~B();
};

struct A {
  ~A() { (void)*f; }
  const B* f;
};

void FN_destructor_order_bad() {
  A a;
  B b;
  a.f = &b;
}

struct A2 {
  ~A2() {}
  const B* f;
};

void destructor_order_empty_destructor_ok() {
  A2 a;
  B b;
  a.f = &b;
}

std::string mk_string();

void variable_init_ternary_ok(bool b) {
  // this can cause issues because of the way the frontend treatment of ternary
  // ?: interacts with the treatment of passing return values by reference as
  // parameters
  std::string newPath = b ? "" : mk_string();
}

void move_data(POD* from, POD* to);

struct Moveable {
  Moveable() = default;
  Moveable(const Moveable&) = delete; // not copyable

  // move constructor
  Moveable(Moveable&& that) noexcept { move_data(&that.data, &data); }

  Moveable& operator=(Moveable&& that) noexcept {
    this->~Moveable();
    ::new (this) Moveable(std::move(that));
    return *this;
  }

  ~Moveable() {}

  Moveable& operator=(const Moveable&) = delete;

  POD data;
};

void move_moveable_ok(Moveable& src) {
  Moveable x;
  x = std::move(src);
}

void placement_new_nothrow_ok() {
  std::unique_ptr<int> p1(new (std::nothrow) int);
  std::unique_ptr<int> p2(new (std::nothrow) int);
}
} // namespace use_after_destructor
