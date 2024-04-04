/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink

class MapExamples {

  companion object {
    fun wrapper(s: String?) {
      inferSensitiveSink(s)
    }
  }

  external fun split1(s: Any?): Map<String, String>

  fun propagateTaintOnMap1Bad_FN() {
    val `object` = inferSecretSource()
    val map = split1(`object`)
    val value = map["Whatever"]
    inferSensitiveSink(value) // Taint flow not reported here
  }

  external fun split2(s: Any?): Map<String, Array<String>>

  // doesn't work with pulse-taint-check-history flag
  fun propagateTaintOnMap2Bad_FN() {
    val `object` = inferSecretSource()
    val map = split2(`object`)
    var value = ""
    if (!map.isEmpty()) {
      val whatever = map["Whatever"]
      if (whatever != null) value = whatever[0]
    }
    inferSensitiveSink(value)
  }

  // doesn't work with pulse-taint-check-history flag
  fun propagateTaintOnMap3Bad_FN() {
    val `object` = inferSecretSource()
    val map = split2(`object`)
    var value: String? = null
    if (!map.isEmpty()) {
      val whatever = map["Whatever"]
      if (whatever != null) value = whatever[0]
    }
    wrapper(value)
  }

  fun propagateTaintOnMap4Bad_FN() {
    val `object` = inferSecretSource()
    val map = split2(`object`)
    var value = ""
    if (!map.isEmpty()) {
      val whatever = map["Whatever"]
      if (whatever != null) value = whatever[0]
    }
    wrapper(value) // Taint flow not reported here
  }
}
