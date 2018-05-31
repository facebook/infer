/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#pragma once

#include_next <iostream>

// this is a marker so that infer can tell whether iostream has been included by
// a give source file
std::ios_base::Init __infer_translation_unit_init_streams;
