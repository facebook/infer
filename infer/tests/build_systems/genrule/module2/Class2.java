/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package genrule.module2;

import genrule.module1.Class1;

public class Class2 {

  void interTargetNPE() {
    Object obj = Class1.returnsNull();
    obj.toString();
  }

  void localNPE2() {
    Object obj = null;
    obj.toString();
  }

}
