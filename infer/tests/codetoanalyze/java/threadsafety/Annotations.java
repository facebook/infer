/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.annotation.concurrent.ThreadSafe;

import android.support.annotation.UiThread;

import com.facebook.infer.annotation.AssumeThreadSafe;
import com.facebook.infer.annotation.Functional;
import com.facebook.infer.annotation.ThreadConfined;

/** tests for classes and method annotations that are meaningful w.r.t thread-safety */

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnBind {
}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnEvent {
}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnMount {
}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnUnbind {
}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnUnmount {
}

interface FunctionalInterface {

  @Functional Object method();
}

@ThreadSafe
class Annotations implements FunctionalInterface {
  Object f;

  @UiThread
  public void setF(Object newF) {
    this.f = newF; // shouldn't report here
  }

  public void callSetFOnMethodOk(Annotations obj) {
    obj.setF(new Object()); // or here
  }

  public void mutateOffUiThreadBad() {
    this.f = new Object();
  }

  // anything annotated with OnEvent is modeled as running on the UI thread, should not warn
  @OnEvent
  public void onClick() {
    this.f = new Object();
  }

  Confined con;

  public void confinedCallerOk(){
    con.foo();
  }

  public void writeFieldOfConfinedClassOk() {
    con.x = 7;
  }

  @ThreadConfined(ThreadConfined.UI)
  class Confined {
    Integer x;

    void foo(){
      x = 22;
    }
  }

  static class Obj {
    Object fld;
  }

  @ThreadConfined(ThreadConfined.ANY) Obj encapsulatedField;

  public void mutateConfinedFieldDirectlyOk() {
    this.encapsulatedField = new Obj();
  }

  public static void mutateConfinedFieldIndirectlyOk(Annotations a) {
    a.encapsulatedField = new Obj();
  }

  public void mutateSubfieldOfConfinedBad() {
    this.encapsulatedField.fld = new Object();
  }

  @ThreadConfined("some_custom_string")
  public void threadConfinedMethodOk() {
    this.f = new Object();
  }

  @OnBind
  public void onBindMethodOk() {
    this.f = new Object();
  }

  @OnMount
  public void onMountMethodOk() {
    this.f = new Object();
  }

  @OnUnmount
  public void onUnmountMethodOk() {
    this.f = new Object();
  }

  @OnUnbind
  public void onUnbindMethodOk() {
    this.f = new Object();
  }

  @AssumeThreadSafe(because = "it's a test")
  public void assumeThreadSafeOk() {
    this.f = new Object();
  }

  @Functional native Object returnFunctional1();
  @Functional Object returnFunctional2() { return null; }
  // marked @Functional in interface
  @Override public Object method() { return null; }

  Object mAssignToFunctional;

  public Object functionalOk1() {
    if (mAssignToFunctional == null) {
      mAssignToFunctional = returnFunctional1();
    }
    return mAssignToFunctional;
  }

  public Object functionalOk2() {
    if (mAssignToFunctional == null) {
      mAssignToFunctional = returnFunctional2();
    }
    return mAssignToFunctional;
  }

  public Object functionalOk3() {
    if (mAssignToFunctional == null) {
      mAssignToFunctional = method();
    }
    return mAssignToFunctional;
  }

  @Functional native double returnDouble();
  @Functional native long returnLong();

  double mDouble;
  long mLong;

  // writes to doubles are not atomic on all platforms, so this is not a benign race
  public double functionalDoubleBad() {
    if (mDouble == 0.0) {
      mDouble = returnDouble();
    }
    return mDouble;
  }

  // writes to longs are not atomic on all platforms, so this is not a benign race
  public long functionaLongBad() {
    if (mLong == 0L) {
      mLong = returnLong();
    }
    return mLong;
  }

  Boolean mBoxedBool;

  @Functional native boolean returnBool();

  public boolean functionalAcrossBoxingOk() {
    if (mBoxedBool != null) {
      mBoxedBool = returnBool();
    }
    return mBoxedBool;
  }

  boolean mBool;

  @Functional native Boolean returnBoxedBool();

  public boolean FP_functionalAcrossUnboxingOk() {
    if (!mBool) {
      mBool = returnBoxedBool();
    }
    return mBool;
  }

  Long mBoxedLong;

  @Functional native Long returnBoxedLong();

  public Long functionalBoxedLongOk() {
    if (mBoxedLong == null) {
      mBoxedLong = returnBoxedLong();
    }
    return mBoxedLong;
  }

  public long functionalAcrossUnboxingLongBad() {
    if (mLong != 0L) {
      mLong = returnBoxedLong();
    }
    return mLong;
  }

  public long FP_functionalAcrossBoxingLongOk() {
    if (mBoxedLong != null) {
      mBoxedLong = returnLong();
    }
    return mBoxedLong;
  }

  public boolean propagateFunctional() {
    return returnBool();
  }

  // show that we can handle indirect returns of procedures marked @Functional
  public void propagateFunctionalOk() {
    boolean returnedFunctional = propagateFunctional();
    mBool = returnedFunctional;
  }

}
