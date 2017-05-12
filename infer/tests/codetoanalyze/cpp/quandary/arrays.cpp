/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace arrays {

// these examples used to crash the HIL conversion
char index_of_literal_ok1() { return "foo"[1]; }

char index_of_literal_ok2() { return "foo"[7]; }

char index_of_literal_ok3(int i) { return "foo"[i]; }
}
