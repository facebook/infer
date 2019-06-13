/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
