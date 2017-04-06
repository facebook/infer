/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include "generic_model.h"

/* This code uses <int> template instantiations, but those are never
  instantiated because there is no implementation of templated code in
  "generic_model.h"
  However, there is instantation for <long long> coming from
  "generic_model.cpp". If generic model is truly generic, then infer will pick
  up specs for those and use them
*/
int genericModelNPE() {
  GenericModelClass<int> x;
  auto ptr = x.get();
  return *ptr;
}

int nonGenericModelNoNPE() {
  NonGenericModelClass<int> x;
  auto ptr = x.get(); // this will be skip function
  return *ptr;
}

int genericModelFunctionNPE() {
  auto ptr = genericModelFunction<int>();
  return *ptr;
}
int nonGenericModelFunctionNPE() {
  auto ptr = nonGenericModelFunction<int>(); // this will be skip function
  return *ptr;
}
