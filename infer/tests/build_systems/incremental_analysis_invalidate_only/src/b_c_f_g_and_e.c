/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "a_and_d.h"

void e() { d(); }

void g() {}

void f() {}

void c() { d(); }

void b() { c(); }
