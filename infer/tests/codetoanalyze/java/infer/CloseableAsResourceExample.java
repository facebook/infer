/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.StringReader;


class LocalException extends IOException {
}

class SomeResource implements Closeable {

  void doSomething() throws LocalException {
    if (!CloseableAsResourceExample.star()) {
      throw new LocalException();
    }
  }

  public void close() {}

  native void foo(int i);
  native static void bar(SomeResource r);

}

class Resource implements Closeable {
  public Resource() {
  }
  public void close() {}
}

class Wrapper implements Closeable {
  Resource mR;
  public Wrapper(Resource r) {
    mR = r;
  }
  public void close() {
    mR.close();
  }
}

class Sub extends Wrapper {
  public Sub(Resource r) {
    super(r);
  }
}

class ResourceWithException implements Closeable {

  public void close() throws IOException {
    if (CloseableAsResourceExample.star()) {
      throw new IOException();
    }
  }
}

public class CloseableAsResourceExample {

  native static boolean star();

  void closingCloseable() {
    SomeResource res = new SomeResource();
    res.close();
  }

  void notClosingCloseable() {
    SomeResource res = new SomeResource();
  }  // should report a resource leak

  void tryWithResource() {
    try (SomeResource res = new SomeResource()) {
      try {
        res.doSomething();
      } catch (LocalException e) {
        // do nothing
      }
    }
  }

  void withException() throws LocalException {
    SomeResource res = new SomeResource();
    res.doSomething();
    res.close();
  } // should report a resource leak


  void closingWrapper() {
    Resource r = new Resource();
    Sub s = new Sub(r);
    s.close();
  }

  void notClosingWrapper() {
    Sub s = new Sub(new Resource());
    s.mR.close();
  }  // should report a resource leak

  void noNeedToCloseStringReader() {
    StringReader stringReader = new StringReader("paf!");
  }

  void noNeedToCloseByteArrayOutputStream() {
    ByteArrayOutputStream stream = new ByteArrayOutputStream(42);
  }

  void noNeedToCloseByteArrayInputStream(byte[] array) {
    ByteArrayInputStream stream = new ByteArrayInputStream(array);
  }

  void closingWithCloseQuietly() {
    SomeResource r = null;
    try {
      r = new SomeResource();
      r.doSomething();
    } catch (IOException e) {
    } finally {
      Utils.closeQuietly(r);
    }
  }

  void failToCloseWithCloseQuietly() {
    try {
      SomeResource r = new SomeResource();
      r.doSomething();
      Utils.closeQuietly(r);
    } catch (IOException e) {
    }
  }

  void noLeakwithExceptionOnClose() throws IOException {
    ResourceWithException res = new ResourceWithException();
    res.close();
  }

  void noLeakWithCloseQuietlyAndExceptionOnClose() {
    ResourceWithException res = new ResourceWithException();
    Utils.closeQuietly(res);
  }

  static T sourceOfNullWithResourceLeak() {
    SomeResource r = new SomeResource();
    return null;
  }

  interface MyCloseable extends Closeable {}

  class MyResource implements MyCloseable {
    public void close() {}
  }

  void leakFoundWhenIndirectlyImplementingCloseable() {
    MyResource res = new MyResource();
  }

  void skippedCallClosesResourceOnArgs() {
    SomeResource res = new SomeResource();
    SomeResource.bar(res);
  }

  void skippedVritualCallDoesNotCloseResourceOnReceiver() {
    SomeResource res = new SomeResource();
    res.foo(42);
  }

}
