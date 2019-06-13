/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#pragma once

// locale_conv in libstdc++ is including 'bits/unique_ptr.h' via
// #include "unique_ptr.h". Infer can't redirect those includes
// so instead include unique_ptr via our header first. Then, it
// won't be included again in 'bits/locale_conv.h'
#include <bits/unique_ptr.h>

#include_next <bits/locale_conv.h>
