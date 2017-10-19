/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// _SHOULD_BE_SKIPPED_

package codetoanalyze.java.checkers;

public class SkippedClass {

  Object f;

  public static SkippedClass returnOwned() {
    return new SkippedClass();
  }

}
