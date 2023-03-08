/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse.sinks;

/**
 * WARNING! These methods are for testing the taint analysis only! Don't use them in models or in
 * real code.
 */
public class InferTaintSinks {

  public static void sink1(Object iMightBeTainted) {}

  public static void sink2(Object iMightBeTainted) {}
}
