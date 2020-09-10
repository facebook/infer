/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
template <typename... Types>
struct Tuple {};

Tuple<Tuple<int>> x;

typedef decltype(nullptr) nullptr_t;
template <nullptr_t X>
struct NullPtrTemplate {};

NullPtrTemplate<nullptr> x1;

template <int x>
struct IntTemplate {};

template <char x>
struct CharTemplate {};
template <long x>
struct LongTemplate {};

IntTemplate<0> x2;
CharTemplate<'c'> x3;
LongTemplate<0x1234567890L> x4;

template <const int *x>
struct X {};
int x5[42];

struct Y {};
template <const Y &b>
struct Z {};
Y y;

template <int (&pa)[5]>
struct W {};
int b[5];

void f(int);
template <void (*pf)(int)>
struct A {};

X<x5> xi;
Z<y> z;
W<b> w;
A<&f> a;
