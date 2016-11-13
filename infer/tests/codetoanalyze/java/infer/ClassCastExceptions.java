/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

class SuperClass {
}

class SubClassA extends SuperClass {
}

class SubClassB extends SuperClass {
}

interface MyInterface {
  public int getInt();
}

class ImplementationOfInterface implements MyInterface {

  public int getInt() {
    return 0;
  }
}

class AnotherImplementationOfInterface implements MyInterface {
  public int getInt() {
    return 1;
  }
}


public class ClassCastExceptions {

  public void classCastException() {
    SuperClass a = new SubClassA();
    SubClassB b = (SubClassB) a;
  }

  public int classCastExceptionImplementsInterfaceCallee(MyInterface i) {
    ImplementationOfInterface impl = (ImplementationOfInterface) i;
    return impl.getInt();
  }

  public int classCastExceptionImplementsInterface() {
    return classCastExceptionImplementsInterfaceCallee(new AnotherImplementationOfInterface());
  }

  public String getURL() {
    return "http://bla.com";
  }

  public void openHttpURLConnection() throws IOException {
    URL url = new URL(getURL());
    HttpURLConnection connection = (HttpURLConnection) url.openConnection();
    connection.disconnect();
  }

  public void castingArrayOfPrimitiveTypeOK(int[] a) {
    int[] b = (int[]) a;
  }

}
