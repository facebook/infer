/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import com.facebook.infer.builtins.InferBuiltins;

public class Builtins {

  void blockError() {
    Object x = null;
    InferBuiltins.assume(x != null);
    x.toString();
  }

  void doNotBlockError(Object x) {
    Object y = null;
    InferBuiltins.assume(x != null);
    y.toString();
  }

  void blockErrorIntAssume(Object x) {
    Object y = null;
    int i = 0;
    InferBuiltins.assume(i != 0);
    y.toString();
  }

  void causeError(Object x) {
    InferBuiltins.assume(x == null);
    x.toString();
  }

}
