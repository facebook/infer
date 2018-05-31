/*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.eradicate;

import javax.annotation.Nullable;

public class NullFieldAccess {
  class C {
    int n;
  }

  interface I {
    @Nullable
    C c = null;
  }


  @Nullable
  C x;
  C y;
  static final
  @Nullable
  C z = null;

  NullFieldAccess() {
    y = new C();
  }

  int useX() {
    C c = x;
    return c.n;
  }

  int useY() {
    C c = y;
    return c.n;
  }

  int useZ() {
    C c = z;
    return c.n;
  }

  int useInterface(I i) {
    C c = i.c;
    return c.n;
  }

  @Nullable Object[] objects;

  int arrayLength() {
    return objects.length;
  }

  Object arrayAccess() {
    return objects[0];
  }
}
