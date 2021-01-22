/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;

class LocalException extends IOException {}

class SomeResource implements Closeable {

  void doSomething() throws LocalException {
    if (!CloseableAsResourceExample.star()) {
      throw new LocalException();
    }
  }

  public void close() {}

  native void foo(int i);

  static native void bar(SomeResource r);
}

class Resource implements Closeable {
  public Resource() {}

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

class ByteArrayOutputStreamWrapper extends ByteArrayOutputStream {}

class ByteArrayInputStreamWrapper extends ByteArrayInputStream {

  public ByteArrayInputStreamWrapper(byte[] arr) {
    super(arr);
  }
}

public class CloseableAsResourceExample {

  static native boolean star();

  void closingCloseable() {
    SomeResource res = new SomeResource();
    res.close();
  }

  void notClosingCloseable() {
    SomeResource res = new SomeResource();
  } // should report a resource leak

  void tryWithResource() {
    try (SomeResource res = new SomeResource()) {
      try {
        res.doSomething();
      } catch (LocalException e) {
        // do nothing
      }
    }
  }

  void withExceptionBad() throws LocalException {
    SomeResource res = new SomeResource();
    res.doSomething();
    res.close();
  } // should report a resource leak

  void closingWrapperOk() {
    Resource r = new Resource();
    Sub s = new Sub(r);
    s.close();
  }

  void notClosingWrapperBad() {
    Sub s = new Sub(new Resource());
    s.mR.close();
  } // should report a resource leak

  void noNeedToCloseStringReaderOk() {
    StringReader stringReader = new StringReader("paf!");
  }

  void noNeedToCloseByteArrayOutputStreamOk() {
    ByteArrayOutputStream stream = new ByteArrayOutputStream(42);
  }

  void noCloseByteArrayWrappersOk(byte[] array) {
    ByteArrayOutputStreamWrapper stream1 = new ByteArrayOutputStreamWrapper();
    ByteArrayInputStreamWrapper stream2 = new ByteArrayInputStreamWrapper(array);
  }

  void noNeedToCloseByteArrayInputStreamOk(byte[] array) {
    ByteArrayInputStream stream = new ByteArrayInputStream(array);
  }

  void closingWithCloseQuietlyOk() {
    SomeResource r = null;
    try {
      r = new SomeResource();
      r.doSomething();
    } catch (IOException e) {
    } finally {
      Utils.closeQuietly(r);
    }
  }

  void failToCloseWithCloseQuietlyBad() {
    try {
      SomeResource r = new SomeResource();
      r.doSomething();
      Utils.closeQuietly(r);
    } catch (IOException e) {
    }
  }

  void noLeakwithExceptionOnCloseOk() throws IOException {
    ResourceWithException res = new ResourceWithException();
    res.close();
  }

  void noLeakWithCloseQuietlyAndExceptionOnCloseOk() {
    ResourceWithException res = new ResourceWithException();
    Utils.closeQuietly(res);
  }

  static T sourceOfNullWithResourceLeakBad() {
    SomeResource r = new SomeResource();
    return null;
  }

  interface MyCloseable extends Closeable {}

  class MyResource implements MyCloseable {
    public void close() {}
  }

  void leakFoundWhenIndirectlyImplementingCloseableBad() {
    MyResource res = new MyResource();
  }

  void skippedCallClosesResourceOnArgsOk() {
    SomeResource res = new SomeResource();
    SomeResource.bar(res);
  }

  void skippedVirtualCallDoesNotCloseResourceOnReceiverOk() {
    SomeResource res = new SomeResource();
    res.foo(42);
  }

  Map returnsLocalMapContainingResourcesOk() {
    HashMap<Integer, Closeable> map = new HashMap<>();
    SomeResource res = new SomeResource();
    Integer key = 42;
    map.put(key, res);
    return map;
  }

  void createsLocalMapContainingResourcesOk() {
    HashMap<Integer, Closeable> map = new HashMap<>();
    SomeResource res = new SomeResource();
    Integer key = 42;
    map.put(key, res);
    map.clear();
  }

  HashMap<Integer, Closeable> resourceMap = new HashMap<>();

  void fieldMapContainingResourcesOk() {
    Integer key = 42;
    SomeResource res = new SomeResource();
    resourceMap.put(key, res);
  }

  // this case is not supported
  void FN_notClearinglocalMapContainingResourcesBad() {
    HashMap<Integer, Closeable> map = new HashMap<>();
    SomeResource res = new SomeResource();
    Integer key = 42;
    map.put(key, res);
  }

  public static void closeCloseable(Closeable closeable) {
    try {
      if (closeable != null) {
        closeable.close();
      }
    } catch (Exception ex) {
    }
  }

  public void finallyCloseOk(File file, String fileContent) {
    if (!file.exists()) {
      FileWriter writer = null;
      try {
        writer = new FileWriter(file);
        writer.write(fileContent);
      } catch (FileNotFoundException e) {
        e.printStackTrace();
      } catch (IOException e) {
        e.printStackTrace();
      } finally {
        closeCloseable(writer);
      }
    }
  }
}
