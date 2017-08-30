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
import genrule.module1.SkipImplementationClass1;

public class Class2 {

  void localNPE2() {
    Object obj = null;
    obj.toString();
  }

  void interTargetNPE() {
    Object obj = Class1.returnsNull();
    obj.toString();
  }

  void interTargetAbstractNPE(Class1 class1) {
    Object obj = class1.abstractMayReturnNull();
    obj.toString();
  }

  void interTargetNativeNPE(Class1 class1) {
    Object obj = class1.nativeMayReturnNull();
    obj.toString();
  }

  void followMethodDeclarationOnlyBad(SkipImplementationClass1 obj1) {
    Object obj2 = obj1.annotatedNullable();
    obj2.toString();
  }

  void followMethodDeclarationOnlyOk(SkipImplementationClass1 obj1) {
    Object obj2 = obj1.notAnnotatedNullable();
    obj2.toString();
  }

}
