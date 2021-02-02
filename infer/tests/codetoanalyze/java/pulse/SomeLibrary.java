/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class SomeLibrary {

  class $$Z {}

  T t;

  T get() {
    return t == null ? null : t;
  }
}
