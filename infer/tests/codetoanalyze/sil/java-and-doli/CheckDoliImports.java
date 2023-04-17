/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* The aim of this file is to demonstrate that the analysis takes
into account the summary of a doli-defined function.
In the particular case, it is the functions
        java.io.File.length
        java.util.Random.nextInt
        codetoanalyze.java.infer.C  (not a library function)
In doli we define these functions to return 2323, and this will trigger
NPEs to be reported.

Moreover, notice that in DoliImports, some functions are defined in
separate doli-rules, and other functions are defined in separate doli-rules*/

package codetoanalyze.java.infer;

import java.io.File;
import java.util.Random;

class A {
  int intField;
}

class B {

  int length() {
    return 420;
  }
}

class C {

  int length() {
    return 420;
  }
}

class CheckDoliImports {

  File fld_F = new File("someFile.txt");

  A fld_A = new A();
  B fld_B = new B();
  C fld_C = new C();

  int m1_ok() {
    if (this.fld_F.length() == 420) {
      fld_A = null;
    }
    return fld_A.intField;
  }

  int m1_bad() {
    if (this.fld_F.length() == 2323) {
      fld_A = null;
    }
    return fld_A.intField; // error
  }

  int m2_ok() {
    if (this.fld_B.length() == 2323) {
      fld_A = null;
    }
    return fld_A.intField;
  }

  int m2_bad() {
    if (this.fld_B.length() == 420) {
      fld_A = null;
    }
    return fld_A.intField; // error
  }

  int m3_ok() {
    if (this.fld_C.length() == 420) {
      fld_A = null;
    }
    return fld_A.intField;
  }

  int m3_bad() {
    if (this.fld_C.length() == 2323) {
      fld_A = null;
    }
    return fld_A.intField; // error
  }

  int m4_ok_FP() {
    java.util.Random randNums = new Random();
    if (randNums.nextInt() == 420) {
      fld_A = null;
    }
    return fld_A.intField;
  }

  int m4_bad() {
    java.util.Random randNums = new Random();
    if (randNums.nextInt() == 2323) {
      fld_A = null;
    }
    return fld_A.intField; // error
  }
}
