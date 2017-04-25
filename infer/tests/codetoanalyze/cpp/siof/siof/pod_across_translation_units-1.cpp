/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

extern int foo();
int bar() { return foo(); }
int baz() { return bar(); }
static int x = baz(); // BAD: report SIOF here
static int x1 = x; // do not report here
static int x2 = x1; // do not report here
