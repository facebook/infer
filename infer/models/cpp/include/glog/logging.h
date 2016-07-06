/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
