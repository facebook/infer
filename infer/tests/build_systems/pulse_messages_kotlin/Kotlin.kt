/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

class Kotlin {

  fun doesNotAcceptNullFirstParam(input: Any) = Unit

  fun doesNotAcceptNullSecondParam(input1: Any?, input2: Any) = Unit
}
