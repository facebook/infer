/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.ThreadConfined;
import com.facebook.infer.annotation.ThreadSafe;

interface UnannotatedInterface {
  public void foo();
}

@ThreadSafe
interface AnnotatedInterface {
  public void foo();
}

interface AnnotatedInterfaceMethod {

  @ThreadSafe
  public void foo();
}

class NotThreadSafe {
  void notThreadSafeOk(UnannotatedInterface i) {
    i.foo(); // ok
  }
}

@ThreadConfined(ThreadConfined.ANY)
interface ThreadConfinedInterface {
  void foo();
}

interface ThreadConfinedMethod {

  @ThreadConfined(ThreadConfined.ANY)
  void foo();
}

@ThreadSafe
public class Dispatch {

  void callUnannotatedInterfaceBad(UnannotatedInterface i) {
    i.foo();
  }

  void callUnannotatedInterfaceIndirectBad(NotThreadSafe s, UnannotatedInterface i) {
    s.notThreadSafeOk(i);
  }

  synchronized void callUnannotatedInterfaceUnderLockOk(NotThreadSafe s, UnannotatedInterface i) {
    s.notThreadSafeOk(i);
  }

  void callAnnotatedInterfaceOk(AnnotatedInterface i) {
    i.foo();
  }

  void callAnnotatedInterfaceMethodOk(AnnotatedInterfaceMethod i) {
    i.foo();
  }

  void callThreadConfinedInterfaceOk(ThreadConfinedInterface t) {
    t.foo();
  }

  void callThreadConfinedInterfaceMethodOk(ThreadConfinedMethod t) {
    t.foo();
  }

  public void callUnderLock(AnnotatedInterface i) {
    synchronized (this) {
      i.foo();
    }
  }
}

class Some {

  void callFromElsewhere(Dispatch d, AnnotatedInterface i) {
    d.callUnderLock(i);
  }
}

@ThreadSafe
class ThreadConfinedField {
  @ThreadConfined(ThreadConfined.ANY)
  UnannotatedInterface mThreadConfined;

  UnannotatedInterface mNormal;

  void interfaceCallOnThreadConfinedFieldOk() {
    mThreadConfined.foo();
  }

  void interfaceCallOnNormalFieldBad() {
    mNormal.foo();
  }
}
