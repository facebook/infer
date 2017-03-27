/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;


import com.facebook.infer.annotation.ThreadSafe;
import com.google.common.annotations.VisibleForTesting;

class ThreadSafeMethods {

  Object field1;
  Object field2;
  Object field3;
  Object field4;
  Object field5;

  @ThreadSafe
  public void threadSafeMethodWriteBad() {
    this.field1 = new Object(); // should warn
  }

  @ThreadSafe
  public Object threadSafeMethodReadBad() {
    return this.field2;
  }

  @ThreadSafe
  private void threadSafePrivateMethodBad() {
    this.field2 = new Object(); // should warn
  }

  @ThreadSafe
  @VisibleForTesting
  public void threadSafeVisibleForTestingMethodBad() {
    this.field3 = new Object(); // should warn
  }

  @ThreadSafe
  public void safeMethodOverride() {
  }

  // won't report this now, but should in the future. if a method annotated with @ThreadSafe
  // in class C touches field f, then all other accesses to f in C must also be thread-safe
  public void FN_writeSameFieldAsThreadSafeMethod1Bad() {
    this.field1 = new Object();
  }

  // reads a field that is written in a method marked thread-safe
  public Object FN_readSameFieldAsThreadSafeMethod1Bad() {
    return this.field1;
  }

  public synchronized void safelyWriteSameFieldAsThreadSafeMethod1Ok() {
    this.field1 = new Object();
  }

  public synchronized Object safelyReadSameFieldAsThreadSafeMethod1Ok() {
    return this.field1;
  }

  @ThreadSafe
  public synchronized void synchronizedWriteOk() {
    this.field4 = new Object();
  }

  // unprotected write to a field that is written safely in a method marked thread-safe
  public void FN_writeSameFieldAsThreadSafeMethod2Bad() {
    this.field4 = new Object();
  }

  // unprotected read of a field that is written safely in a method marked thread-safe
  public Object FN_readSameFieldAsThreadSafeMethod2Bad() {
    return this.field4;
  }

  @ThreadSafe
  public synchronized Object synchronizedReadOk() {
    return this.field5;
  }

  // unprotected write to a field that is read safely in a method marked thread-safe
  public void FN_writeSameFieldAsThreadSafeMethod3Bad() {
    this.field5 = new Object();
  }

  // unprotected read of a field that is read safely in a method marked thread-safe
  public Object FN_readSameFieldAsThreadSafeMethod3Bad() {
    return this.field5;
  }

}

class ThreadSafeMethodsSubclass extends ThreadSafeMethods {
  Object subclassField;

  @Override
  // overrides method annotated with @ThreadSafe, should warn
  public void safeMethodOverride() {
    this.subclassField = new Object();
  }

  public void FN_writeThreadSafeFieldOfSuperclassBad() {
    this.field1 = new Object();
  }

  public Object FN_readThreadSafeFieldOfSuperclassBad() {
    return this.field1;
  }

  public void FN_writeThreadSafeFieldOfOverrideBad() {
    this.subclassField = new Object();
  }

  public Object FN_readThreadSafeFieldOfOverrideBad() {
    return this.subclassField;
  }

}
