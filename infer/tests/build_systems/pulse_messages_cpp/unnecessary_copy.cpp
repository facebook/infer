/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <optional>
#include <string>
#include <vector>

struct A {
  std::vector<int> vec;
};

A& get_a_ref() {
  static A static_a;
  return static_a;
}

std::vector<int> copy_decl_bad() {
  auto cpy = get_a_ref(); // unnecessary copy, use a ref
  // call to copy constructor A::A(a, n$0)
  return cpy.vec;
}

void copy_assignment_bad(A source) {
  A c; // default constructor is called
  c = source; // copy assignment operator is called
}

class Test {
  A mem_a;

  void unnecessary_copy_moveable_bad(A&& a) { mem_a = a; }

  void intermediate_member_field_copy_bad() {
    A a;
    std::vector<A> singleton = {a};
  }
};

int get_size(A a) { return a.vec.size(); }

void unnecessary_intermediate_copy(const A& my_a) {
  // we are copying the argument my_a unnecessarily because get_size doesn't
  // have const A&
  get_size(my_a);
}

class MyValueOr {
  bool b;
  A& value;

 public:
  A value_or(const A& default_value) const {
    if (b) {
      return value;
    } else {
      return default_value;
    }
  }
};

void call_value_or_bad(const MyValueOr& c) {
  const static A f{};
  A g = c.value_or(f);
}

namespace ns {

template <typename X>
X creates_copy(X a) {
  return X{a};
}
} // namespace ns

void intermediate_copy_via_model_bad(A arr) {
  get_size(ns::creates_copy(
      arr)); // creates an intermediate copy (via model/unknown)
}

void get_optional_value(std::optional<A> x) {}

void call_get_optional_value_bad(A x) {
  x.vec[0] = 42;
  get_optional_value(x);
}

class UnMovable {
 public:
  std::vector<int> vec_;
  // by defining a destructor but no move constructor, there is no
  // auto-generated move constructor
  ~UnMovable() = default;

  void unnecessary_copy_assignment_const_bad(const std::vector<int> vec) {
    vec_ = vec;
  }
};

void call_by_value(UnMovable arg){};

void no_move_intermediate_bad(UnMovable unmovable) {
  call_by_value(
      std::move(unmovable)); // we can't suggest moving here since UnMovable
                             // doesn't have move constructor
}

class ConstTest {
  A test_field;

  int unnecessary_copy_intermediate_const_bad(const A my_a) {
    return get_size(my_a);
  }

  int unnecessary_copy_intermediate_const_move_bad(const A my_a) {
    return get_size(std::move(my_a));
  }

  void unnecessary_copy_assignment_const_move_bad(const A my_a) {
    test_field = std::move(my_a);
  }
};

template <typename X>
class MyValueOr2 {
  bool b;
  X& value;

 public:
  X value_or(const X& default_value) const {
    if (b) {
      return value;
    } else {
      return default_value;
    }
  }
};

void call_value_or2_bad(const MyValueOr2<A>& c) {
  const static A f{};
  A g = c.value_or(f);
}
