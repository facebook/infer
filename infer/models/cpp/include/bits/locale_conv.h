/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#pragma once

// locale_conv in libstdc++ is including 'bits/unique_ptr.h' via
// #include "unique_ptr.h". Infer can't redirect those includes
// so instead include unique_ptr via our header first. Then, it
// won't be included again in 'bits/locale_conv.h'
#include <bits/unique_ptr.h>

#include_next <bits/locale_conv.h>
