/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

class TaintFormals {
  // first parameter is tainted based on the config
  fun firstParameterTainted(tainted: Any, notTainted: Any) {
    // should be tainted
    InferTaint.inferSensitiveSink(tainted)
    // should not be tainted
    InferTaint.inferSensitiveSink(notTainted)
  }

  fun callbackAnonymousClassTaintedBad() {
    InferTaint.addCallback(
        object : Callback {
          // result parameter is tainted based on the config
          override fun onCompletion(result: Any) {
            InferTaint.inferSensitiveSink(result)
          }
        })
  }

  fun callbackLambdaTaintedBad() {
    InferTaint.addCallback { result ->
      // result parameter is tainted based on the config
      InferTaint.inferSensitiveSink(result)
    }
  }
}
