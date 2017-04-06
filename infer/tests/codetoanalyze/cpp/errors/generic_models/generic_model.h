/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
template <class T>
struct __attribute__((annotate("__infer_generic_model"))) GenericModelClass {
  T* get();
};

template <class T>
struct NonGenericModelClass {
  T* get();
};

template <class T>
__attribute__((annotate("__infer_generic_model"))) T* genericModelFunction();

template <class T>
T* nonGenericModelFunction();
