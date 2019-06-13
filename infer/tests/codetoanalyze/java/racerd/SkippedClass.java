/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// _SHOULD_BE_SKIPPED_

package codetoanalyze.java.checkers;

public class SkippedClass {

  Object f;

  public static SkippedClass returnOwned() {
    return new SkippedClass();
  }
}
