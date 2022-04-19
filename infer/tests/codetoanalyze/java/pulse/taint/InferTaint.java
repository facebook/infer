/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

/**
 * WARNING! These methods are for testing the taint analysis only! Don't use them in models or in
 * real code.
 */
public class InferTaint {

  public static Object object_undefined() {
    return new Object();
  }

  // these are to test whether we can add a taint spec to methods that have an implementation
  public static Object inferSecretSource() {
    Object o = object_undefined();
    return o;
  }

  public static void inferSensitiveSink(Object iMightBeTainted) {}

  public static Object inferUniversalSanitizer(Object iMightBeTainted) {
    return iMightBeTainted;
  }

  // these are to test whether we can add a taint spec to undefined methods
  public static native Object inferSecretSourceUndefined();

  public static native void inferSensitiveSinkUndefined(Object iMightBeTainted);
}
