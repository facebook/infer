/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  public void safeMethodOverride() {}

  // if a method annotated with @ThreadSafe in class C writes field f, then all other accesses to f
  // in C must also be thread-safe
  public void writeSameFieldAsThreadSafeMethod1Bad() {
    // warn here because field1 is also written in @ThreadSafe method threadSafeMethodWriteBad
    this.field1 = new Object();
  }

  // reads a field that is written in a method marked thread-safe
  public Object readSameFieldAsThreadSafeMethod1Bad() {
    return this.field1;
  }

  // TODO: should we report this or not?
  public synchronized void safelyWriteSameFieldAsThreadSafeMethod1Ok() {
    this.field1 = new Object();
  }

  public synchronized Object readSameFieldAsThreadSafeMethodWhileSynchronized1Bad() {
    return this.field1;
  }

  @ThreadSafe
  public synchronized void synchronizedWriteOk() {
    this.field4 = new Object();
  }

  // unprotected write to a field that is written safely in a method marked thread-safe
  public void writeSameFieldAsThreadSafeMethod2Bad() {
    this.field4 = new Object();
  }

  // unprotected read of a field that is written safely in a method marked thread-safe
  public Object readSameFieldAsThreadSafeMethod2Bad() {
    return this.field4;
  }

  @ThreadSafe
  public synchronized Object FN_synchronizedReadBad() {
    return this.field5;
  }

  private void privateAccessOk() {
    this.field5 = new Object();
  }

  // unprotected write to a field that is read safely in a method marked thread-safe
  public void FN_writeSameFieldAsThreadSafeMethod3Bad() {
    this.field5 = new Object();
  }

  // none of the writes are marked thread-safe/locked, no reason to report
  public Object readSameFieldAsThreadSafeMethodOk() {
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

  public void writeThreadSafeFieldOfOverrideBad() {
    this.subclassField = new Object();
  }

  public Object readThreadSafeFieldOfOverrideBad() {
    return this.subclassField;
  }
}
