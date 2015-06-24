/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package codetoanalyze.java.infer;

import com.facebook.infer.models.InferBuiltins;

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
