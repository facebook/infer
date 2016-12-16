/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

@ThreadSafe
public class ThreadSafeExample{

  /*Included to make sure infer does not report on class initializers*/
  static Class<?> A = ThreadSafeExample.class;

  Integer f;

  public ThreadSafeExample() {
    f = 86;
  }

  public void tsOK() {
    synchronized (this) {
      f = 42;
    }
  }

  public void tsBad() {
    f = 24;
  }

  public void recursiveBad() {
    f = 44;
    recursiveBad();
  }

  private void evenOk() {
    f = 44;
    oddBad();
  }

  public void oddBad() {
    evenOk(); // should report here
  }

  // shouldn't report here because it's a private method
  private void assignInPrivateMethodOk() {
    f = 24;
  }

  // but should report here, because now it's called
  public void callPublicMethodBad() {
    assignInPrivateMethodOk();
  }

  private void callAssignInPrivateMethod() {
    assignInPrivateMethodOk();
  }

  // should report a deeperTraceBade -> callAssignInPrivateMethod -> assignInPrivateMethodOk trace
  public void deeperTraceBad() {
    callAssignInPrivateMethod();
  }

  public synchronized void callFromSynchronizedPublicMethodOk() {
    assignInPrivateMethodOk();
  }

  private synchronized void synchronizedCallerOk() {
    assignInPrivateMethodOk();
  }

  public void callFromUnsynchronizedPublicMethodOk() {
    synchronizedCallerOk();
  }

  // although the constructor touches f, we shouldn't complain here
  public void callConstructorOk() {
    new ThreadSafeExample();
  }

  private Object returnConstructorOk() {
    return new ThreadSafeExample();
  }

  public void transitivelyCallConstructorOk() {
    returnConstructorOk();
  }

}

class ExtendsThreadSafeExample extends ThreadSafeExample{

  Integer field;

  /* Presently,we will warn not just on overwridden methods from
  @ThreadSafe class, but potentially on other methods in subclass */
  public void newmethodBad() {
     field = 22;
  }

  /* Bad now that it's overridden */
  public void tsOK() {
     field = 44;
  }

}

@NotThreadSafe
class NotThreadSafeExtendsThreadSafeExample extends ThreadSafeExample{

  Integer field;

/* We don't want to warn on this */
  public void newmethodBad() {
     field = 22;
  }

}

@ThreadSafe
class YesThreadSafeExtendsNotThreadSafeExample extends NotThreadSafeExtendsThreadSafeExample{

  Integer subsubfield;

/* We do want to warn on this */
  public void subsubmethodBad() {
     subsubfield = 22;
  }

}
