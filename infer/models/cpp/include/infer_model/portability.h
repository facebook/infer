/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once
// TODO set it in configure script instead
// This is hacky attempt to follow what folly does
// https://github.com/facebook/folly/blob/b1eb6819f3ffe6b645f39d505ca8ace3116b7873/folly/configure.ac#L232
#if !defined(INFER_USE_LIBCPP) && defined(__APPLE__)
#define INFER_USE_LIBCPP 1
#elif defined(FOLLY_USE_LIBCPP)
#define INFER_USE_LIBCPP 1
#endif

// Follow what folly does - gnu libstdc++ implementation is different from
// llvm's libc++. This way folly can forward declare decls from std library
// even when they are infer models
// https://github.com/facebook/folly/blob/b1eb6819f3ffe6b645f39d505ca8ace3116b7873/folly/Portability.h#L253-L255
#if INFER_USE_LIBCPP
#include <__config>
#define INFER_NAMESPACE_STD_BEGIN _LIBCPP_BEGIN_NAMESPACE_STD
#define INFER_NAMESPACE_STD_END _LIBCPP_END_NAMESPACE_STD
#else
#define INFER_NAMESPACE_STD_BEGIN namespace std {
#define INFER_NAMESPACE_STD_END }
#endif
