/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

namespace infer_traits {
// all friends of this class and will be translated as type T by infer
// frontend instead of their own type
template <class T>
class TranslateAsType {};
} // namespace infer_traits

#define INFER_MODEL_AS_DEREF_FIRST_ARG \
  __attribute__((annotate("__infer_replace_with_deref_first_arg"))) {}
