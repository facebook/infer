/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;
import javax.annotation.Nullable;

public class NullableSuggest {
  private Object obj0;
  @Nullable private Object obj1;

  private static class OtherClass {
    private Object obj2;
    @Nullable private Object obj3;
  }

  public void noAssignNullOk() {
  }

  public void assignNotNullOk() {
    obj0 = new Object();
    obj1 = new Object();
  }

  public void assignNullBad() {
    obj0 = null;
  }

  public void assignNullToNullableOk() {
    obj1 = null;
  }

  public void assignNullToFieldInOtherClassBad() {
    OtherClass oc = new OtherClass();
    oc.obj2 = null;
  }

  public void assignNullToNullableFieldInOtherClassOk() {
    OtherClass oc = new OtherClass();
    oc.obj3 = null;
  }
}
