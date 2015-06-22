// Copyright (c) 2015-Present Facebook. All rights reserved.

package codetoanalyze.java.infer;

import java.io.Closeable;
import java.io.IOException;

public class CloseableAsResourceExample {

  class LocalException extends IOException {
  }

  class SomeResource implements Closeable {

      native boolean isValid();
      void doSomething() throws LocalException {
        if (!isValid()) {
          throw new LocalException();
        }
      }
      public void close() {}
  }

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

  class Res implements Closeable {
    public Res() {
    }
    public void close() {}
  }

  class Wrapper implements Closeable {
    Res mR;
    public Wrapper(Res r) {
      mR = r;
    }
    public void close() {
      mR.close();
    }
  }

  class Sub extends Wrapper {
    public Sub(Res r) {
      super(r);
    }
  }

  void closingWrapper() {
    Res r = new Res();
    Sub s = new Sub(r);
    s.close();
  }

  void notClosingWrapper() {
    Sub s = new Sub(new Res());
    s.mR.close();
  }  // should report a resource leak

}
