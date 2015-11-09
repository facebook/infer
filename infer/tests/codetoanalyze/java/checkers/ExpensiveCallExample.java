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

public class ExpensiveCallExample {

  Object mObject;

  @Expensive
  void expensiveMethod() {
    mObject = new Object();
  }

  @PerformanceCritical
  void shouldReportExpensiveCallWarning() {
    expensiveMethod();
  }

}
