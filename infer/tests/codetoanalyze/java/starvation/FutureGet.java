/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import java.util.concurrent.Future;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import android.support.annotation.UiThread;
import java.util.concurrent.TimeUnit;

class FutureGet {
  Future future;
  Object lock;

  @UiThread
  void getDirectBad() throws InterruptedException, ExecutionException {
    future.get();
  }

  @UiThread
  void getIndirectBad() {
    synchronized(lock) {}
  }

  void getUnderLock() throws InterruptedException, ExecutionException {
    synchronized(lock) {
      future.get();
    }
  }

  void getOnOtherThreadOk() throws InterruptedException, ExecutionException {
    future.get();
  }

  @UiThread
  void getTimeoutOneDayBad() throws InterruptedException, ExecutionException {
    try {
      future.get(1L, TimeUnit.DAYS);
    } catch (TimeoutException e) {}
  }

  @UiThread
  void getTimeoutOneSecondOk() throws InterruptedException, ExecutionException {
    try {
      future.get(1L, TimeUnit.SECONDS);
    } catch (TimeoutException e) {}
  }

  @UiThread
  void getTimeoutOneHourBad() throws InterruptedException, ExecutionException {
    try {
      future.get(1L, TimeUnit.HOURS);
    } catch (TimeoutException e) {}
  }

  @UiThread
  void getTimeoutFourSecondsOk() throws InterruptedException, ExecutionException {
    try {
      future.get(4L, TimeUnit.SECONDS);
    } catch (TimeoutException e) {}
  }

  @UiThread
  void getTimeout4999MilliSecondsOk() throws InterruptedException, ExecutionException {
    try {
      future.get(4999L, TimeUnit.MILLISECONDS);
    } catch (TimeoutException e) {}
  }

  @UiThread
  void getTimeout50000001MicroSecondsBad() throws InterruptedException, ExecutionException {
    try {
      future.get(5000001L, TimeUnit.MICROSECONDS);
    } catch (TimeoutException e) {}
  }
}
