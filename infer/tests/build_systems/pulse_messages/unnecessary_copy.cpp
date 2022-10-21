/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
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
    std::vector<A> singleton = {mem_a};
  }
};

int get_size(A a) { return a.vec.size(); }

void unnecessary_intermetidate_copy(const A& my_a) {
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
