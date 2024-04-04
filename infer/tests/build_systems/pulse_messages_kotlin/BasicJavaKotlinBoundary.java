/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse;

public class BasicJavaKotlinBoundary {

  Object returnsNull() {
    return null;
  }

  Object returnsNotNull() {
    return new Object();
  }

  void passNullWhenDisallowedBad1(Kotlin kotlin) {
    Object input = null;
    kotlin.doesNotAcceptNullFirstParam(input);
  }

  void passNullWhenAllowedOk(Kotlin kotlin) {
    Object input1 = null;
    Object input2 = new Object();
    kotlin.doesNotAcceptNullSecondParam(input1, input2);
  }

  void passNullWhenDisallowedBad2(Kotlin kotlin) {
    Object input1 = null;
    Object input2 = null;
    kotlin.doesNotAcceptNullSecondParam(input1, input2);
  }

  void passNullWhenDisallowedInterprocBad1(Kotlin kotlin) {
    Object input = returnsNull();
    kotlin.doesNotAcceptNullFirstParam(input);
  }

  void passNullWhenAllowedInterprocOk(Kotlin kotlin) {
    Object input1 = returnsNull();
    Object input2 = returnsNotNull();
    kotlin.doesNotAcceptNullSecondParam(input1, input2);
  }

  void passNullWhenDisallowedInterprocBad2(Kotlin kotlin) {
    Object input1 = returnsNull();
    Object input2 = returnsNull();
    kotlin.doesNotAcceptNullSecondParam(input1, input2);
  }
}
