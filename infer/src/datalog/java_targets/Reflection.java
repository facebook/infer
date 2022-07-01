/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.lang.reflect.*;
import java.util.Stack;

public class Reflection {
  public static void main(String args[]) {
    dumpMethods();
    allocNewInstance();
    allocNewClass();
  }

  private static void dumpMethods() {
    try {
      Class c = Class.forName("java.util.Stack");
      Method m[] = c.getDeclaredMethods();
      for (int i = 0; i < m.length; i++) System.out.println(m[i].toString());
    } catch (Throwable e) {
      System.err.println(e);
    }
  }

  private static void allocNewInstance() {
    try {
      Class c = Class.forName("java.util.Stack");
      Object s = c.newInstance();
      System.out.println(((Stack) s).empty());
    } catch (Throwable e) {
      System.err.println(e);
    }
  }

  private static void allocNewClass() {
    try {
      Stack s = new Stack();
      Class c = s.getClass();
      System.out.println(c.toString());
    } catch (Throwable e) {
      System.err.println(e);
    }
  }
}
