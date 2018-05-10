/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

typedef __attribute__((ext_vector_type(2))) float vec_float2;

vec_float2 simple(vec_float2 vec) { return vec.xy; }
