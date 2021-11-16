/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <variant>
#include <vector>

namespace std_visit {
struct A {
  int s_;
  ~A() {}
  A() {}
};

// See https://en.cppreference.com/w/cpp/utility/variant/visit for more details
using var_t = std::variant<float, int, A>;

template <class... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...)->overloaded<Ts...>;

void visit_ok(var_t x) {
  std::visit(overloaded{
                 [](float a) {},
                 [](int i) {},
                 [](const A& a) {},
             },
             x);
}

void visit_all_ok(std::vector<var_t>& vec) {
  for (auto& x : vec) {
    // the overloaded { .. } construct appears as an InitListExpr in
    // the AST that the frontend needs to translate. Otherwise, since
    // children of that InitListExpr create C++ temporaries, pulse
    // will complain that they get destroyed because it never sees
    // them being created (this is only a problem in a loop where the
    // same temporary is re-used without having been re-initialized).
    std::visit(overloaded{
                   [](float a) {},
                   [](int i) {},
                   [](const A& a) {},
               },
               x);
  }
}

} // namespace std_visit
