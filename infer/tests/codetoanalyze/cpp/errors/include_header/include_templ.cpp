/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "header2.h"

using namespace header2;

// instantiate templates to produce bug reports for them
void div0_B_int() {
  B<int> b;
  b.div0();
}

void div0_B_A() {
  B<A> b;
  b.div0();
}

void div0_templ_int() { div0_templ<int>(); }

int div0_templ_A() { div0_templ<A>(); }
