/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.crashcontext;

import java.lang.reflect.Method;

public class NativeMethodExample {

  public static void foo() {
    String s = null;
    s.toString();
  }

  public static void main(String[] args) {
    try {
      // Calling method.invoke is a reliable way of getting a native method
      // in the stack (from the implementation of reflection) between this
      // method and the target of the reflective invocation.
      Method method = NativeMethodExample.class.getDeclaredMethod("foo");
      Object o = method.invoke(new Object[] {});
    } catch (ReflectiveOperationException e) {
      throw new Error(e);
    }
  }
}
