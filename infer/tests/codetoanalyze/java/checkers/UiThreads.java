/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import android.support.annotation.UiThread;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface AnyThread {}

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface ForUiThread {}

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface ForNonUiThread {}

public class UiThreads {

  @UiThread
  void uiThread() {}

  @AnyThread
  void anyThread() {}

  @ForUiThread
  void forUiThread() {}

  @ForNonUiThread
  void forNonUiThread() {}

  @ForUiThread
  void callForNonUiThreadBad1() {
    forNonUiThread();
  }

  @UiThread
  void callForNonUiThreadBad2() {
    forNonUiThread();
  }

  @AnyThread
  void callUiThreadBad1() {
    uiThread();
  }

  @ForNonUiThread
  void callUiThreadBad2() {
    uiThread();
  }

  @ForUiThread
  void callUiThreadOk() {
    uiThread();
  }

  @UiThread
  void callForUiThreadOk() {
    forUiThread();
  }

  @ForNonUiThread
  void callAnyThreadOk1() {
    anyThread();
  }

  @ForUiThread
  void callAnyThreadOk2() {
    anyThread();
  }

  @UiThread
  void callAnyThreadOk3() {
    anyThread();
  }

  @AnyThread
  void callForNonUiThreadOk() {
    forNonUiThread();
  }
}
