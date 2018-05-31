/*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.eradicate;

import javax.annotation.Nullable;

class SubclassExample {

  class T {
    public void f() {
    }
  }

  class A {

    public T foo() {
      return new T();
    }

    public
    @Nullable
    T bar() {
      return null;
    }

    public void deref(@Nullable T t) {
      if (t != null) {
        t.f();
      }
    }

    public void noDeref(T t) {
    }

  }

  class B extends A {

    public
    @Nullable
    T foo() {
      return null;
    }

    public T bar() {
      return new T();
    }

  }

  interface I {
    public T baz();
  }

  class C implements I {

    public
    @Nullable
    T baz() {
      return null;
    }
  }

  class D extends A {

    public void deref(T t) {
      t.f();
    }

    public void noDeref(@Nullable T t) {
      if (t != null) {
        t.f();
      }
    }

  }
}

class ConstructorsAreExcluded {
  class Base {
    Base (@Nullable String s) {
    }
  }

  class Derived extends Base {
    Derived (String s) { // OK: there's no sub-typing between constructors
      super(s);
    }
  }
}

public class InconsistentSubclassAnnotation implements InconsistentSubclassAnnotationInterface {

  public static void callFromSuperclass(SubclassExample.A a) {
    SubclassExample.T t = a.foo();
    t.f();
  }

  public static void callWithNullableParam(SubclassExample.A a, @Nullable SubclassExample.T t) {
    a.deref(t);
  }

  public String implementInAnotherFile(String s) {
    return "";
  }

}
