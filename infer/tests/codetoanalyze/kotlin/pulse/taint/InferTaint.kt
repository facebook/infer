/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse
/**
 * WARNING! These methods are for testing the taint analysis only! Don't use them in models or in
 * real code.
 */
object InferTaint {

  fun object_undefined(): Any {
    return Any()
  }

  // these are to test whether we can add a taint spec to methods that have an implementation
  fun inferSecretSource(): Any {
    return object_undefined()
  }

  fun inferSensitiveSink(iMightBeTainted: Any?) {}

  fun inferUniversalSanitizer(iMightBeTainted: Any): Any {
    return iMightBeTainted
  }

  // these are to test whether we can add a taint spec to undefined methods
  external fun inferSecretSourceUndefined(): Any

  external fun inferSensitiveSinkUndefined(iMightBeTainted: Any?)

  // these are to tests that only calls of functions with the same names from InferTaintSinks are
  // recognized as sinks based on class_name_regex config
  fun sink1(iMightBeTainted: Any?) {}

  fun sink2(iMightBeTainted: Any?) {}

  fun addCallback(callback: Callback) {}

  // fun addCallback(callback: Function<Any>) {}
}
