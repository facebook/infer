/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

// TODO fix FN T38248006
@ThreadSafe
public class NonSourceVar {
  private long field;

  public void FN_conditionalOperatorBad(long v) {
    field = field < v ? field : v;
  }
}
