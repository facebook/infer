/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Casts {
  // there is no way to test this dynamically, as to avoid an exception
  // we must first create a CastsC object.  So this is more for documenting.
  public static void main(String args[]) {
    CastsA.downcast(new CastsC());
  }
}

class CastsA {
  static CastsC downcast(CastsB c) {
    return (CastsC) c;
  }

  static boolean checkclass(CastsD d) {
    return d instanceof CastsE;
  }
}

class CastsB {}

class CastsC extends CastsB {}

class CastsD {}

class CastsE extends CastsD {}
