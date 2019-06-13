/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;
import com.google.common.util.concurrent.Futures;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

class FutureGet {
  Future future;
  Object lock;

  @UiThread
  void getDirectBad() throws InterruptedException, ExecutionException {
    future.get();
  }

  @UiThread
  void getIndirectBad() {
    synchronized (lock) {
    }
  }

  void getUnderLock() throws InterruptedException, ExecutionException {
    synchronized (lock) {
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
    } catch (TimeoutException e) {
    }
  }

  @UiThread
  void getTimeoutOneSecondOk() throws InterruptedException, ExecutionException {
    try {
      future.get(1L, TimeUnit.SECONDS);
    } catch (TimeoutException e) {
    }
  }

  @UiThread
  void getTimeoutOneHourBad() throws InterruptedException, ExecutionException {
    try {
      future.get(1L, TimeUnit.HOURS);
    } catch (TimeoutException e) {
    }
  }

  @UiThread
  void getTimeoutFourSecondsOk() throws InterruptedException, ExecutionException {
    try {
      future.get(4L, TimeUnit.SECONDS);
    } catch (TimeoutException e) {
    }
  }

  @UiThread
  void getTimeout4999MilliSecondsOk() throws InterruptedException, ExecutionException {
    try {
      future.get(4999L, TimeUnit.MILLISECONDS);
    } catch (TimeoutException e) {
    }
  }

  @UiThread
  void getTimeout50000001MicroSecondsBad() throws InterruptedException, ExecutionException {
    try {
      future.get(5000001L, TimeUnit.MICROSECONDS);
    } catch (TimeoutException e) {
    }
  }

  @UiThread
  void getTimeout64BitsBad() throws InterruptedException, ExecutionException {
    try {
      future.get(9223372036854775807L, TimeUnit.MICROSECONDS);
    } catch (TimeoutException e) {
    }
  }

  @UiThread
  Object getFuturesDoneOk(Future<Object> future) throws ExecutionException {
    return Futures.getDone(future);
  }
}
