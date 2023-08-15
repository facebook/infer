/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink
import java.util.function.Function

class Lambdas {
  fun createFunctionWithTaintedParam(): Function<Int, String> {
    val obj = inferSecretSource()
    return Function { n: Int ->
      inferSensitiveSink(obj)
      n.toString()
    }
  }

  fun invokeFunction(function: Function<Int, String>): String {
    return function.apply(42)
  }

  fun createAndInvokeFunctionBad(): String {
    val function = createFunctionWithTaintedParam()
    return invokeFunction(function)
  }

  fun createAndInvokeFunctionBad2(): String {
    val function = createFunctionWithTaintedParam()
    return function.apply(42)
  }

  fun createFunctionFromAnonymousClassWithTaintedParam(): Function<Int, String> {
    val obj = inferSecretSource()
    return Function { n ->
      inferSensitiveSink(obj)
      n.toString()
    }
  }

  fun createAndInvokeFunctionFromAnonymousClassBad(): String {
    val function = createFunctionFromAnonymousClassWithTaintedParam()
    return invokeFunction(function)
  }

  fun createAndInvokeFunctionFromAnonymousClassBad2(): String {
    val function = createFunctionFromAnonymousClassWithTaintedParam()
    return function.apply(42)
  }
}
