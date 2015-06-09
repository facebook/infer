package codetoanalyze.java.infer;

public class SomeLibrary {

  T t;

  T get() {
    return t == null ? null : t;
  }

}
