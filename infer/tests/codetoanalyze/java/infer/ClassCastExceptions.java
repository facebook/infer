/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
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

interface MyInterface {
  public int getInt();
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

}
