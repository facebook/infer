/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.support.annotation.NonNull;
import android.widget.EditText;
import com.facebook.infer.annotation.Assertions;
import com.facebook.infer.annotation.Initializer;
import com.facebook.infer.annotation.SuppressViewNullability;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.inject.Inject;

// for butterknife
@interface Bind {}

public class FieldNotInitialized {

  String a;

  @Nullable String b;

  @Nonnull String c; // Means: assume it will be initialized to a nonnull value somewhere else.

  @Inject String d; // Means: assume it will be initialized via dependency injection

  @NonNull String e;

  @Bind EditText f; // Means: assume it will be initialized, and ignore null assignment

  @SuppressViewNullability EditText g;

  //  Eradicate should only report one initialization error
  FieldNotInitialized() {}

  void testNullifyFields() {
    f = null; // OK  the framework could write null into the field
    g = null; // OK  the framework could write null into the field
  }

  class OnlyRead {
    Object o;

    OnlyRead() {
      Object x = o; // not initialized
    }
  }

  class WriteItself {
    Object o;

    WriteItself() {
      o = o; // not initialized
    }
  }

  class Swap {
    Object o1;
    Object o2;

    Swap() {
      o1 = o2; // not initialized
      o2 = new Object();
    }
  }

  class SwapOK {
    Object o1;
    Object o2;

    SwapOK() {
      o1 = new Object();
      o2 = o1;
    }
  }

  class OnlyReadIndirect {
    Object o1;
    Object o2;

    private void indirect() {
      Object x = o1; // not initialized
      o2 = new Object();
    }

    OnlyReadIndirect() {
      indirect();
    }
  }

  class ConditionalFieldInit {
    Object o1;
    @Nullable Object o2 = null;

    public ConditionalFieldInit() {
      if (o2 != null) {
        o1 = new Object(); // Not always initialized
      }
    }
  }

  class InitIfNull {
    Object o;

    public InitIfNull() {
      if (o == null) o = new Object();
    }
  }

  class InitIfNull2 {
    Object o;

    public InitIfNull2(Object x) {
      if (o == null) o = x;
    }
  }

  class InitIfNull3 {
    Object o;

    Object getNotNull() {
      return new Object();
    }

    public InitIfNull3() {
      if (o == null) o = getNotNull();
    }
  }

  class InitCircular {
    String s;

    InitCircular() {
      String tmp = s;
      s = tmp; // s is not initialized: circular initialization
    }
  }

  class InitWithOtherClass {
    class OtherClass {
      String s = "";
    }

    String s;

    InitWithOtherClass(OtherClass x) {
      s = x.s;
    }
  }

  class InitWithThisClass {

    String s;

    InitWithThisClass(InitWithThisClass x) {
      s = x.s;
    }
  }
}

/**
 * If a method is marked with @Initializer annotation, we essentially treat is as a constuctror: if
 * a field is initialized in one of such methods, we assume this method will be called before using
 * the field, so we don't consider it "not initialized" error. A popular usecase for that is a
 * Builder pattern, when required fields are set not in the constuctor, but in corresponding
 * setters, and then build() method checks in runtime that all fields are initialized.
 */
class TestInitializerAnnotation {
  String initInConstructorIsOK;
  String initInInitilizerMethod1IsOK;
  String initInInitilizerMethod2IsOK;
  String initInAnyOtherMethodIsBAD;
  String initByNullableInInitializedMethodIsBAD;
  String dontInitAtAllIsBAD;
  @Nullable String dontInitOptionalIsOK;

  TestInitializerAnnotation() {
    initInConstructorIsOK = "";
  }

  @Initializer
  void set1(String value) {
    // OK: we assume set1() will be called before the class is actually used
    this.initInInitilizerMethod1IsOK = value;
  }

  @Initializer
  void set2(String value) {
    // OK: we assume set2() will be called before the class is actually used
    this.initInInitilizerMethod2IsOK = value;
  }

  void set3(String value) {
    // BAD: though the field is initialized here, set3 is not marked as @Initialized
    this.initInAnyOtherMethodIsBAD = value;
  }

  @Initializer
  void set4(@Nullable String value) {
    // BAD: method is marked as @Initializer, but the value can be null
    this.initByNullableInInitializedMethodIsBAD = value;
  }

  // Example of a typical usecase:
  // a build() method that is supposed to be called before the class is used.
  Object build() {
    // Fail hard if the required fields are not initialzed.
    // Unfortunately, this will lead to "condition redundant" warnings, despite the fact
    // that checking for this makes total sense.
    // TODO(T53531699) don't issue "condition redundant" warning in this case.
    Assertions.assertCondition(
        initInInitilizerMethod1IsOK != null && initInInitilizerMethod2IsOK != null);

    // ... do some stuff

    // return some meaninful object
    return "";
  }
}
