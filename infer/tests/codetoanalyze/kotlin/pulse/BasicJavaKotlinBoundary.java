/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse;

public class BasicJavaKotlinBoundary {

  public static String returnsNullString() {
    return null;
  }

  public static Object returnsNull() {
    return null;
  }

  public static Object returnsNotNull() {
    return new Object();
  }

  void passNullWhenAllowedOk1(Kotlin kotlin) {
    Object input = null;
    kotlin.acceptsNull(input);
  }

  void passNotNullOk1(Kotlin kotlin) {
    Object input = new Object();
    kotlin.acceptsNull(input);
  }

  void passNullWhenAllowedOk2(Kotlin kotlin) {
    Object input1 = null;
    Object input2 = new Object();
    kotlin.paramsWithDifferentNullness(input1, input2);
  }

  void passNotNullOk2(Kotlin kotlin) {
    Object input1 = new Object();
    Object input2 = new Object();
    kotlin.paramsWithDifferentNullness(input1, input2);
  }

  void passNotNullOk3(Kotlin kotlin) {
    Object input = new Object();
    kotlin.doesNotAcceptNull(input);
  }

  void passNullWhenDisallowedBad1(Kotlin kotlin) {
    Object input = null;
    kotlin.doesNotAcceptNull(input);
  }

  void passNullWhenDisallowedBad2(Kotlin kotlin) {
    Object input1 = new Object();
    Object input2 = null;
    kotlin.paramsWithDifferentNullness(input1, input2);
  }

  void passNullWhenDisallowedBad3(Kotlin kotlin) {
    Object input1 = null;
    Object input2 = null;
    kotlin.paramsWithDifferentNullness(input1, input2);
  }

  void passNullWhenAllowedInterprocOk1(Kotlin kotlin) {
    Object input = returnsNull();
    kotlin.acceptsNull(input);
  }

  void passNotNullInterprocOk1(Kotlin kotlin) {
    Object input = returnsNotNull();
    kotlin.acceptsNull(input);
  }

  void passNullWhenAllowedInterprocOk2(Kotlin kotlin) {
    Object input1 = returnsNull();
    Object input2 = returnsNotNull();
    kotlin.paramsWithDifferentNullness(input1, input2);
  }

  void passNotNullInterprocOk2(Kotlin kotlin) {
    Object input1 = returnsNotNull();
    Object input2 = returnsNotNull();
    kotlin.paramsWithDifferentNullness(input1, input2);
  }

  void passNotNullInterprocOk3(Kotlin kotlin) {
    Object input = returnsNotNull();
    kotlin.doesNotAcceptNull(input);
  }

  void passNullWhenDisallowedInterprocBad1(Kotlin kotlin) {
    Object input = returnsNull();
    kotlin.doesNotAcceptNull(input);
  }

  void passNullWhenDisallowedInterprocBad2(Kotlin kotlin) {
    Object input1 = returnsNotNull();
    Object input2 = returnsNull();
    kotlin.paramsWithDifferentNullness(input1, input2);
  }

  void passNullWhenDisallowedInterprocBad3(Kotlin kotlin) {
    Object input1 = returnsNull();
    Object input2 = returnsNull();
    kotlin.paramsWithDifferentNullness(input1, input2);
  }
}
