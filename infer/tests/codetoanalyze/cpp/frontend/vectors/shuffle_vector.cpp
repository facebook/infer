/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef __attribute__((ext_vector_type(2))) float vec_float2;

vec_float2 simple(vec_float2 vec) { return vec.xy; }
