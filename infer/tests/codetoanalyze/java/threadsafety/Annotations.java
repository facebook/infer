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

@ThreadSafe
class Annotations {
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

  public void confinedCaller(){
    con.foo();
  }

  @ThreadConfined
  class Confined {
    Integer x;

    void foo(){
      x = 22;
    }
  }

  static class Obj {
    Object fld;
  }

  @ThreadConfined Obj encapsulatedField;

  public void mutateConfinedFieldDirectlyOk() {
    this.encapsulatedField = new Obj();
  }

  public static void mutateConfinedFieldIndirectlyOk(Annotations a) {
    a.encapsulatedField = new Obj();
  }

  public void mutateSubfieldOfConfinedBad() {
    this.encapsulatedField.fld = new Object();
  }

  @ThreadConfined
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

}
