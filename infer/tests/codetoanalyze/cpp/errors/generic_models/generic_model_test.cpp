/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "generic_model.h"

/* This code uses <int> template instantiations, but those are never
  instantiated because there is no implementation of templated code in
  "generic_model.h"
  However, there is instantation for <long long> coming from
  "generic_model.cpp". If generic model is truly generic, then infer will pick
  up specs for those and use them
*/
/* FIXME(t17253769): flaky because OnDemand doesn't know that
 * `genericModelFunction<int>()` and `genericModelFunction<long long>()` are
 * meant to be the same function. These two procedures have the same specs
 * filename, so depending on which gets analyzed first the spec may already be
 * there or not, but if it's the latter then OnDemand will not know to schedule
 * the analysis of `genericModelFunction<long long>()` */
// int genericModelNPE() {
//   GenericModelClass<int> x;
//   auto ptr = x.get();
//   return *ptr;
// }

int nonGenericModelNoNPE() {
  NonGenericModelClass<int> x;
  auto ptr = x.get(); // this will be skip function
  return *ptr;
}

/* FIXME(t17253769) commented out for flakiness (see above) */
// int genericModelFunctionNPE() {
//   auto ptr = genericModelFunction<int>();
//   return *ptr;
// }

int nonGenericModelFunctionNoNPE() {
  auto ptr = nonGenericModelFunction<int>(); // this will be skip function
  return *ptr;
}
