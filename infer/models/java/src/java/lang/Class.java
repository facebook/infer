/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

public final class Class<T> {

  transient String name;

  public String getName() {
    return this.name;
  }

  public static Class<?> forName(String className) throws ClassNotFoundException {
    return new Class();
  }

  public boolean isAssignableFrom(Class<?> cls) {
    return false;
  }

  public static Class getPrimitiveClass(String name) {
    Class c = new Class();
    c.name = name;
    return c;
  }
}
