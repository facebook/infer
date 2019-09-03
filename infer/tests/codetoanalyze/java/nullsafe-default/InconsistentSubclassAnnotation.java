/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import external.library.SomeExternalClass;
import javax.annotation.Nullable;

class SubclassExample {

  class T {
    public void f() {}
  }

  class A {

    public T foo() {
      return new T();
    }

    public @Nullable T bar() {
      return null;
    }

    public void deref(@Nullable T t) {
      if (t != null) {
        t.f();
      }
    }

    public void noDeref(T t) {}
  }

  class B extends A {

    public @Nullable T foo() {
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

    public @Nullable T baz() {
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
    Base(@Nullable String s) {}
  }

  class Derived extends Base {
    Derived(String s) { // OK: there's no sub-typing between constructors
      super(s);
    }
  }
}

class ExtendsExternalLibrary extends SomeExternalClass {

  @Override
  public @Nullable Object externalMethod1() {
    // subtyping error on the return type not reported as we cannot
    // rely on the external libraries to be correctly annotated
    return null;
  }

  @Override
  public void externalMethod2(Object object) {
    // subtyping error on the parameter type are reported
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

  public @Nullable Object overloadedMethod() {
    return null;
  }

  public Object overloadedMethod(Object object) {
    return object;
  }
}

class Super {
  String overloadingMethodLookupFP(int i) {
    return Integer.toString(i);
  }
}

class Sub extends Super {
  @Nullable
  String overloadingMethodLookupFP(Object object) {
    return null;
  }
}
