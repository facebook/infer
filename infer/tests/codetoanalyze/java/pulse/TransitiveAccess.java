/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class TransitiveAccess {
  public static class Sinks {
    public static void safe() {}

    public static void sink() {}
  }

  public static class Base {}

  public static class Context extends Base {
    public static void sourceOk() {
      Sinks.safe();
    }

    public static void sourceBad() {
      Sinks.sink();
    }
  }
}
