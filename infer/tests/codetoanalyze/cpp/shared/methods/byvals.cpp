/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <utility>

namespace pass_by_val {

struct PlainStruct {
  int x;
  int* y;
};
PlainStruct dummy_struct{0, nullptr};

int plain_struct_by_val(PlainStruct p) { return p.x + *(p.y); }
int plain_struct_by_ref(PlainStruct& lref,
                        PlainStruct&& rref,
                        PlainStruct* ptr) {
  return lref.x + rref.x + ptr->x;
}

typedef PlainStruct PlainStructTypeDef1;
using PlainStructTypeDef2 = PlainStructTypeDef1;
typedef PlainStruct* PlainStructPtrTypeDef1;
using PlainStructPtrTypeDef2 = const PlainStruct*;

int type_alias_by_val(PlainStructTypeDef1 p1, PlainStructTypeDef2 p2) {
  return plain_struct_by_val(p1) + plain_struct_by_val(p2);
}
int type_alias_by_ref(PlainStructPtrTypeDef1 p1, PlainStructPtrTypeDef2 p2) {
  return p1->x + p2->x;
}

int decltype_by_val(decltype(dummy_struct) p) { return p.x + *(p.y); }
// decltype(paren_expr) follows different type deduction rules
int decltype_by_ref(decltype((dummy_struct)) p) { return p.x + *(p.y); }

template <typename T>
struct Id {
  using Result = T;

  Id(T, T&, T&&) {}
};

int dependent_by_val(Id<PlainStruct>::Result p) { return p.x + *(p.y); }
int dependent_by_ref(Id<const PlainStruct&>::Result p) { return p.x + *(p.y); }

double to_double(int x) { return x; }
template <int N>
struct Tricky {
  using Result = decltype(to_double(N));
};
double tricky_dependent_by_val(Tricky<8>::Result t) { return t; }

template <typename T, typename... Args>
Id<T> make_id(Args&&... args) {
  return Id<T>(std::forward<Args>(args)...);
}

Id<int> perfect_forwarding_by_ref() {
  int a = 0, b = 1;
  return make_id<int>(a, b, 2);
}

} // namespace pass_by_val
