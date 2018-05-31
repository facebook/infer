/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once
// This is a hacky attempt to follow what folly does
// https://github.com/facebook/folly/blob/b1eb6819f3ffe6b645f39d505ca8ace3116b7873/folly/configure.ac#L232
// Depending whether the project is compiled with libc++ or stdlibc++, include
// their internal config headers to get information about versions
// clang-format off
#if __has_include(<__config>) // defines _LIBCPP_VERSION
#include <__config>
#elif __has_include(<bits/c++config.h>) // defines __GLIBCXX__
#include <bits/c++config.h>
#endif
// clang-format on

// Figure out whether the library really supports c++11 standard
#if __cplusplus >= 201103L
#if __GLIBCXX__ >= 20130531
// C++11 is really supported from gcc 4.8.1 onward.
#define INFER_CPP11_ON 1
#elif defined _LIBCPP_VERSION // for now assume libc++ always supported c++11
#define INFER_CPP11_ON 1
#endif
#endif // __cplusplus >= 201103L

#if !defined(INFER_USE_LIBCPP) && defined(_LIBCPP_VERSION)
#define INFER_USE_LIBCPP 1
#elif defined(FOLLY_USE_LIBCPP) // follow folly configuration if it's available
#define INFER_USE_LIBCPP 1
#endif

// Follow what folly does - gnu libstdc++ implementation is different from
// llvm's libc++. This way folly can forward declare decls from std library
// even when they are infer models
// https://github.com/facebook/folly/blob/b1eb6819f3ffe6b645f39d505ca8ace3116b7873/folly/Portability.h#L253-L255
// On top of that, define platform-dependent STL iterators.
#if INFER_USE_LIBCPP
#include <__config>
#define INFER_NAMESPACE_STD_BEGIN _LIBCPP_BEGIN_NAMESPACE_STD
#define INFER_NAMESPACE_STD_END _LIBCPP_END_NAMESPACE_STD
#define STD_ITER(T, C) __wrap_iter<T>
#else
#define INFER_NAMESPACE_STD_BEGIN namespace std {
#define INFER_NAMESPACE_STD_END }
#define STD_ITER(T, C) __gnu_cxx::__normal_iterator<T, C>
#endif
