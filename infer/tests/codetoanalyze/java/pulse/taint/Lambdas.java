/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.util.function.Function;

public class Lambdas {

  Function<Integer, String> createFunctionWithTaintedParam() {
    Object object = InferTaint.inferSecretSource();
    return n -> {
      InferTaint.inferSensitiveSink(object);
      return String.valueOf(n);
    };
  }

  String invokeFunction(Function<Integer, String> function) {
    return function.apply(42);
  }

  String createAndInvokeFunctionBad() {
    Function<Integer, String> function = createFunctionWithTaintedParam();
    return invokeFunction(function);
  }

  String createAndInvokeFunctionBad2() {
    Function<Integer, String> function = createFunctionWithTaintedParam();
    return function.apply(42);
  }

  Function<Integer, String> createFunctionFromAnonymousClassWithTaintedParam() {
    Object object = InferTaint.inferSecretSource();
    return new Function<Integer, String>() {
      @Override
      public String apply(Integer n) {
        InferTaint.inferSensitiveSink(object);
        return String.valueOf(n);
      }
    };
  }

  String createAndInvokeFunctionFromAnonymousClassBad() {
    Function<Integer, String> function = createFunctionFromAnonymousClassWithTaintedParam();
    return invokeFunction(function);
  }

  String createAndInvokeFunctionFromAnonymousClassBad2() {
    Function<Integer, String> function = createFunctionFromAnonymousClassWithTaintedParam();
    return function.apply(42);
  }
}
