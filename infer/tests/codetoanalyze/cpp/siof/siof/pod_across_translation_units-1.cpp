/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

extern int foo();
int bar() { return foo(); }
int baz() { return bar(); }
static int x = baz(); // BAD: report SIOF here
static int x1 = x; // do not report here
static int x2 = x1; // do not report here
