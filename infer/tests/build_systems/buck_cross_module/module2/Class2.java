/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package crossmodule.module2;

import crossmodule.module1.Class1;

public class Class2 {

  void crossModuleNPE1() {
    Class1.returnsNull().toString();
  }

  void crossModuleNPE2(Class1 c) {
    c.nullableField.toString();
  }

}
