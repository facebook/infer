/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
abstract class StringLiteral {
  abstract String source(String type);

  abstract void sink(String s);

  void fOk() {
    sink("ok");
  }

  void fBad() {
    sink("bad");
  }

  void gBad() {
    gLatent("bad");
  }

  void gLatent(String s) {
    sink(s);
  }

  void hOk() {
    sink(source("ok"));
  }

  void hBad() {
    sink(source("bad"));
  }
}
