/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* The aim of this file is to demonstrate that the analysis takes
into account the summary of a doli-defined function.
In the particular case, it is the function
        java.io.File.length
In doli we define it to return 2323, and this will trigger a
NPE to be reported in CheckSimpleDoliImport.m1_bad() */

package codetoanalyze.java.infer;

import java.io.File;

class A {
  int intField;
}

public class CheckSimpleDoliImport {

  File f = new File("whatever.txt");

  A localA = new A();

  int m1_bad() {
    if (this.f.length() == 2323) {
      localA = null;
    }
    return localA.intField;
    // The doli model _should_  cause an error report
    // The Java model will _not_ cause error report,
    // because we do not yet have a model for the function length
  }

  int m2_ok() {
    if (this.f.length() != 2323) {
      localA = null;
    }
    return localA.intField;
    // The doli model should _not_ cause an error report
    // The Java model will _not_ cause error report,
    // because we do not yet have a model for the function length

  }
}
