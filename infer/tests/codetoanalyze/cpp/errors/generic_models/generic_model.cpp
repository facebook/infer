/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include "generic_model.h"

template <class T>
T* GenericModelClass<T>::get() {
  return nullptr;
}

template <class T>
T* NonGenericModelClass<T>::get() {
  return nullptr;
}

template <class T>
T* genericModelFunction() {
  return nullptr;
}

template <class T>
T* nonGenericModelFunction() {
  return nullptr;
}

// explicit instantiations with <long long> as template argument
template class GenericModelClass<long long>;
template class NonGenericModelClass<long long>;
template long long* genericModelFunction<long long>();
template long long* nonGenericModelFunction<long long>();
