/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

class Kotlin {

  fun acceptsNull(input: Any?) = Unit

  fun doesNotAcceptNull(input: Any) = Unit

  fun paramsWithDifferentNullness(input1: Any?, input2: Any) = Unit

  fun passNullFromJavaWhenAllowedOk() {
    val input = BasicJavaKotlinBoundary.returnsNull()
    acceptsNull(input)
  }

  fun passNotNullFromJavaOk() {
    val input = BasicJavaKotlinBoundary.returnsNotNull()
    acceptsNull(input)
    doesNotAcceptNull(input)
  }

  fun passNullFromJavaWhenDisallowedBad() {
    val input = BasicJavaKotlinBoundary.returnsNull()
    doesNotAcceptNull(input)
  }

  fun passNullWhenDisallowedThirdPartyBad_FN() {
    val str = BasicJavaKotlinBoundary.returnsNullString()
    val regex = Regex(".*")
    regex.find(str, startIndex = 0)
  }
}
