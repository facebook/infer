/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
