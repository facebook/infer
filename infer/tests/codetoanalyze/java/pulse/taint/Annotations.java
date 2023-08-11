/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

class Annotations {

  static class Methods {

    @SensitiveSourceMarker
    String markedSource() {
      return "";
    }

    @SensitiveSinkMarker
    void markedSink(String input) {}

    @SanitizerMarker
    String markedSanitizer(String input) {
      return input;
    }

    String unmarkedSource() {
      return "";
    }

    void unmarkedSink(String input) {}

    String unmarkedSanitizer(String input) {
      return input;
    }

    void markedSourceToMarkedSinkBad() {
      markedSink(markedSource());
    }

    void markedSourceUsingMarkedSanitizerToMarkedSinkOk() {
      markedSink(markedSanitizer(markedSource()));
    }

    void markedSourceUsingUnmarkedSanitizerToMarkedSinkBad() {
      markedSink(unmarkedSanitizer(markedSource()));
    }

    void unmarkedSourceToMarkedSinkOk() {
      markedSink(unmarkedSource());
    }

    void unmarkedSourceUsingMarkedSanitizerToMarkedSinkOk() {
      markedSink(markedSanitizer(unmarkedSource()));
    }

    void markedSourceToUnmarkedSinkOk() {
      unmarkedSink(markedSource());
    }
  }

  static class Fields {

    @SensitiveSourceMarker static String markedSource;

    @SensitiveSinkMarker static String markedSink;

    static String unmarked;

    void markedSourceToMarkedSinkBad() {
      markedSink = markedSource;
    }

    void unmarkedSourceToMarkedSinkOk() {
      markedSink = unmarked;
    }

    void markedSourceToUnmarkedSinkOk() {
      unmarked = markedSource;
    }
  }
}
