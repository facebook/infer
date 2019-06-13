/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package genrule.module2;

import genrule.annotations.Nullable;
import genrule.module1.Class1;
import genrule.module1.SkipImplementationClass1;

public class Class2 {

  @Nullable Object field;

  void localNPE2Bad() {
    Object obj = null;
    obj.toString();
  }

  void interTargetNPEBad() {
    Object obj = Class1.returnsNull();
    obj.toString();
  }

  void interTargetAbstractNPEBad(Class1 class1) {
    Object obj = class1.abstractMayReturnNull();
    obj.toString();
  }

  void interTargetNativeNPEBad(Class1 class1) {
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

  void dereferenceLocalNullableFieldBad() {
    field.toString();
  }

  void dereferenceInterTargetField1Bad(Class1 class1) {
    class1.field1.toString();
  }

  int dereferenceInterTargetField2Bad(Class1 class1) {
    return class1.field2.x;
  }

  void dereferenceUnannotatedMethodReturningNullBad(Class1 class1) {
    class1.unannotatedReturnNull().toString();
  }

  static class Sub extends Class1.Sub {
    @Override
    public @Nullable Object subtypingInconsistency(Object object) {
      return null;
    }
  }
}
