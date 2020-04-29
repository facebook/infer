/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
