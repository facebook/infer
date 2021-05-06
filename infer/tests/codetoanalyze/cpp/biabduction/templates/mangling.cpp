/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// test that templated types are sufficiently unique'd by the frontend, so that
// it doesn't conflate different types

namespace mangling {

template <typename... Types>
struct Tuple {};

int bad_packed_templates() {
  Tuple<Tuple<int>> x;
  return 1 / 0;
}

typedef decltype(nullptr) nullptr_t;
template <nullptr_t X>
struct NullPtrTemplate {};

int bad_nullptr_templates() {
  NullPtrTemplate<nullptr> x1;
  return 1 / 0;
}

template <int x>
struct IntTemplate {};

template <char x>
struct CharTemplate {};
template <long x>
struct LongTemplate {};

int bad_integral_types_templates() {
  IntTemplate<0> x2;
  CharTemplate<'c'> x3;
  LongTemplate<1234567890L> x4;
  return 1 / 0;
}

template <const int* pci>
struct PointerTypeTemplate {};
int array_is_pointer_type[10];

template <int (&pa)[5]>
struct PointerTypeTemplate2 {};
int array_of_size_5[5];

template <void (*pf)(int)>
struct FunctionPointerTemplate {};
void some_fun(int);

struct SomeStruct {};
template <const SomeStruct& b>
struct ReferenceTypeTemplate {};
SomeStruct some_struct;

int bad_reference_and_pointer_templates() {
  PointerTypeTemplate<array_is_pointer_type> a;
  PointerTypeTemplate2<array_of_size_5> c;
  FunctionPointerTemplate<&some_fun> d;
  ReferenceTypeTemplate<some_struct> b;
  return 1 / 0;
}

} // namespace mangling
