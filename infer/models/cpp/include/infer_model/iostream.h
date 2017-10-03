/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#pragma once

#include_next <iostream>

// this is a marker so that infer can tell whether iostream has been included by
// a give source file
std::ios_base::Init __infer_translation_unit_init_streams;
