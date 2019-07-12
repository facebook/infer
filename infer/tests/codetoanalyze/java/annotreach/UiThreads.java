/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import android.support.annotation.MainThread;
import android.support.annotation.UiThread;
import android.support.annotation.WorkerThread;
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

  @MainThread
  void mainThread() {}

  @AnyThread
  void anyThread() {}

  @ForUiThread
  void forUiThread() {}

  @ForNonUiThread
  void forNonUiThread() {}

  @WorkerThread
  void workerThread() {}

  void unannotated() {}

  void callUiThreadMethod() {
    uiThread();
    mainThread();
    forUiThread();
  }

  void callNonUiThreadMethod() {
    forNonUiThread();
    workerThread();
  }

  @UiThread
  void callsFromUiThreadBad() {
    callNonUiThreadMethod();
  }

  @UiThread
  void callsFromUiThreadOk() {
    callUiThreadMethod();
    anyThread();
    unannotated();
  }

  @MainThread
  void callsFromMainThreadBad() {
    callNonUiThreadMethod();
  }

  @MainThread
  void callsFromMainThreadOk() {
    callUiThreadMethod();
    anyThread();
    unannotated();
  }

  @ForUiThread
  void callsFromForUiThreadBad() {
    callNonUiThreadMethod();
  }

  @ForUiThread
  void callsFromForUiThreadOk() {
    callUiThreadMethod();
    anyThread();
    unannotated();
  }

  @ForNonUiThread
  void callsFromNonUiThreadBad() {
    callUiThreadMethod();
  }

  @ForNonUiThread
  void callsFromNonUiThreadOk() {
    callNonUiThreadMethod();
    anyThread();
    unannotated();
  }

  @WorkerThread
  void callsFromWorkerThreadBad() {
    callUiThreadMethod();
  }

  @WorkerThread
  void callsFromWorkerThreadOk() {
    callNonUiThreadMethod();
    anyThread();
    unannotated();
  }

  @AnyThread
  void callsFromAnyThreadBad() {
    callUiThreadMethod();
    callNonUiThreadMethod();
  }

  @AnyThread
  void callsFromAnyThreadOk() {
    anyThread();
    unannotated();
  }
}
