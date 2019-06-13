/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "initializer.h"

extern int foo();

static int x = foo() + 5;
static int y = x + z + 1;
