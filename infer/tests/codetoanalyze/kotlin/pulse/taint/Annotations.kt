/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

internal class Annotations {

  @SensitiveSourceMarker fun markedSource() = ""

  @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.EMAIL) fun email() = ""

  @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.LOCATION) fun location() = ""

  @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.OTHER) fun other() = ""

  @SensitiveSinkMarker fun markedSink(input: String?) {}

  @SanitizerMarker fun markedSanitizer(input: String): String = input

  fun unmarkedSource() = ""

  fun unmarkedSink(input: String?) {}

  fun unmarkedSanitizer(input: String): String = input

  fun markedSourceToMarkedSinkBad() {
    markedSink(markedSource())
  }

  fun markedSourceUsingMarkedSanitizerToMarkedSinkOk() {
    markedSink(markedSanitizer(markedSource()))
  }

  fun markedSourceUsingUnmarkedSanitizerToMarkedSinkBad() {
    markedSink(unmarkedSanitizer(markedSource()))
  }

  fun unmarkedSourceToMarkedSinkOk() {
    markedSink(unmarkedSource())
  }

  fun unmarkedSourceUsingMarkedSanitizerToMarkedSinkOk() {
    markedSink(markedSanitizer(unmarkedSource()))
  }

  fun markedSourceToUnmarkedSinkOk() {
    unmarkedSink(markedSource())
  }

  fun unmarkedSourceToUnmarkedSinkOk() {
    unmarkedSink(unmarkedSource())
  }

  fun emailToSinkBad() {
    markedSink(email())
  }

  fun locationToSinkBad() {
    markedSink(location())
  }

  fun otherToSinkOk() {
    markedSink(other())
  }
}
