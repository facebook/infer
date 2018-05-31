/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#pragma once

// <atomic> in libstdc++ is including 'bits/atomic_base.h', which gets included
// by other library files as well. Override this header with <atmoic> to avoid
// the problem
#include <atomic>
