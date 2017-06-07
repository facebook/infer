/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import android.support.v4.app.FragmentActivity;
import android.widget.ImageView;
import android.view.View;

import com.facebook.infer.annotation.Expensive;
import com.facebook.infer.annotation.PerformanceCritical;
import javax.annotation.Nullable;

interface AnnotatedInterface {

  @PerformanceCritical
  void annotatedPerformanceCriticalInInterface();

}


class Other {

  @Expensive
  void expensive() {
  }

  void callsExpensive1() {
    expensive();
  }

  void inexpensiveMethod() {
  }

}

@Expensive
class ExpensiveClass {

  void anExpensiveMethod() {
  }

}

@PerformanceCritical
class PerformanceCriticalClass {

  void performanceCriticalMethod1(ExpensiveClass c) {
    c.anExpensiveMethod(); // should report
  }

  void performanceCriticalMethod2(Other o) {
    o.expensive(); // should report
  }

  void performanceCriticalMethod3(Other o) {
    o.callsExpensive1(); // should report
  }

  void performanceCriticalMethod4(Other o) {
    o.inexpensiveMethod(); // should not report
  }


}

class ExpensiveSubclass extends ExpensiveClass {

  void anotherExpensiveMethod() {
  }

}

class PerformanceCriticalSubclass extends PerformanceCriticalClass {

  void subclassPerformanceCriticalMethod1(ExpensiveClass c) {
    c.anExpensiveMethod(); // should report
  }

  void subclassPerformanceCriticalMethod2(ExpensiveSubclass c) {
    c.anotherExpensiveMethod(); // should report
  }

  void subclassPerformanceCriticalMethod3(Other o) {
    o.callsExpensive1(); // should report;
  }

  void subclassPerformanceCriticalMethod4(Other o) {
    o.inexpensiveMethod(); // should not report;
  }

}


public class ExpensiveCallExample implements AnnotatedInterface {

  @Nullable Other mOther;

  void nonExpensiveMethod() {}

  @Expensive
  void expensiveMethod() {
    // The checker should still report the expensive call stack despite the call cycle
    methodWrapper();
  }

  void methodWrapper() {
    expensiveMethod();
  }

  @PerformanceCritical
  void notCallingExpensiveMethod() {
    nonExpensiveMethod();
  }

  @PerformanceCritical
  void directlyCallingExpensiveMethod() {
    expensiveMethod();
  }

  @PerformanceCritical
  void indirectlyCallingExpensiveMethod() {
    methodWrapper();
  }

  @PerformanceCritical
  void callingExpensiveMethodFromInterface(ExpensiveInterfaceExample object) {
    object.m5();
  }

  void callsExpensive2() {
    mOther.callsExpensive1();
  }

  @PerformanceCritical
  void longerCallStackToExpensive() {
    callsExpensive2();
  }

  @PerformanceCritical
  View callsFindViewByIdFromView(ImageView view, int id) {
    return view.findViewById(id);
  }

  @PerformanceCritical
  View callsFindViewByIdFromActivity(FragmentActivity activity, int id) {
    return activity.findViewById(id);
  }

  @PerformanceCritical
  void callMethodOnExpensiveClass(ExpensiveClass c) {
    c.anExpensiveMethod();
  }

  public void annotatedPerformanceCriticalInInterface() {
    mOther.callsExpensive1();
  }

  @PerformanceCritical
  void callExpensiveMethodWithUnlikely() {
    if (Branch.unlikely(mOther != null)) {
      mOther.callsExpensive1();
    }
  }

  @PerformanceCritical
  void onlyOneExpensiveCallUsingUnlikely() {
    if (Branch.unlikely(mOther != null)) {
      mOther.callsExpensive1();
    }
    expensiveMethod();
  }

  @PerformanceCritical
  void callsExpensiveInTheUnlikelyElseBranch() {
    if (Branch.unlikely(mOther != null)) {
      // Do nothing
    } else  {
      expensiveMethod();
    }
  }

  native boolean test();

  @PerformanceCritical
  void callsExpensiveWithDisjunctionAfterUnlikely() {
    if (Branch.unlikely(mOther != null) || test()) {
      expensiveMethod();
    }
  }

  @PerformanceCritical
  void callsExpensiveWithUnlikelyInLocalVariable() {
    boolean b = Branch.unlikely(mOther != null);
    if (b) {
      expensiveMethod();
    }
  }

  @PerformanceCritical
  void callsExpensiveWithOverriddenUnlikelyCondition() {
    boolean b = Branch.unlikely(mOther != null);
    b = test();
    if (b) {
      expensiveMethod();
    }
  }

  @PerformanceCritical
  void callsExpensiveInConditionalBranch() {
    if (test()) {
      expensiveMethod();
    }
  }

}
