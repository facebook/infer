/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// glog/logging library has functionally equivalent but simpler
// definitions of some CHECK_* macros when STATIC_ANALYSIS is defined.
// Since infer wants those definitions, define the macro before including
// glog/logging.h
#ifndef STATIC_ANALYSIS
#define STATIC_ANALYSIS
#include_next <glog/logging.h>
#undef STATIC_ANALYSIS
#else
#include_next <glog/logging.h>
#endif
