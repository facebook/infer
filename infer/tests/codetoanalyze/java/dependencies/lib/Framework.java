/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package lib;

public class Framework {

  public static Object returnNull() {
    return null;
  }

  private static Str source() {
    return Str.ATTACKER_CONTROLLED;
  }

  public static Str getStr() {
    return source();
  }

  public static Str readFile(Str s) {
    try (MyStream inputStream = new MyStream(s)) {
      return inputStream.readContent();
    }
  }
}
