/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.Expensive;
import com.facebook.infer.annotation.PerformanceCritical;

public class AnnotationReachabilityDuplicatesExample {

  @Expensive
  native void expensive();

  void callsExpensive1() {
    expensive();
  }

  void callsExpensive2() {
    expensive();
  }

  void callsExpensiveTwice() {
    callsExpensive1();
    callsExpensive2();
  }

  @PerformanceCritical
  void perfCriticalBad2() {
    callsExpensiveTwice(); // should report here only once
  }

  native boolean star();

  void callsEitherExpensive() {
    if (star()) {
      callsExpensive1();
    } else {
      callsExpensive2();
    }
  }

  @PerformanceCritical
  void perfCriticalBad1() {
    callsEitherExpensive(); // should report here only once
  }
}
