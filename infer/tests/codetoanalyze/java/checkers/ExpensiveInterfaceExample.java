/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.Expensive;
import com.facebook.infer.annotation.PerformanceCritical;


public interface ExpensiveInterfaceExample {

  interface I {

    @PerformanceCritical
    public void m1();

    public void m2();

  }

  class C {

    public void m3() {}

    public void m4() {}

  }

  @Expensive
  public void m5();

  interface I2 extends I {
    @PerformanceCritical void m3();
  }


  abstract class ImplementsInterface implements I2 {

    @Expensive void expensive() {}

    @Override public void m1() {
      expensive();
    }
  }

}
