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

    @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.EMAIL)
    String email() {
      return "";
    }

    @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.LOCATION)
    String location() {
      return "";
    }

    @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.OTHER)
    String other() {
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

    void emailToSinkBad() {
      markedSink(email());
    }

    void locationToSinkBad() {
      markedSink(location());
    }

    void otherToSinkOk() {
      markedSink(other());
    }
  }

  static class Fields {

    @SensitiveSourceMarker static String markedSource;

    @SensitiveSinkMarker static String markedSink;

    @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.EMAIL)
    static String email;

    @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.LOCATION)
    static String location;

    @SensitiveSourceMarkerWithValue(SensitiveSourceMarkerType.OTHER)
    static String other;

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

    void emailToMarkedSinkBad() {
      markedSink = email;
    }

    void locationToMarkedSinkBad() {
      markedSink = location;
    }

    void otherToMarkedSinkOk() {
      markedSink = other;
    }
  }
}
