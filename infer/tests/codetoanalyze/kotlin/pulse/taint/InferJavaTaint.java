/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.kotlin.pulse;

/**
 * WARNING! These methods are for testing the taint analysis only! Don't use them in models or in
 * real code.
 */
public class InferJavaTaint {

  public static Object inferSecretSource() {
    return new Object();
  }

  public static void inferSensitiveSink(Object iMightBeTainted) {}
}
