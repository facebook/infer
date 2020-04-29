/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.os.Bundle;
import android.support.annotation.NonNull;
import com.facebook.infer.annotation.Assertions;
import com.facebook.infer.annotation.Initializer;
import com.facebook.infer.annotation.SuppressFieldNotInitialized;
import com.facebook.infer.annotation.SuppressViewNullability;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.inject.Inject;

// for butterknife
@interface Bind {}

public class FieldNotInitialized {

  // different ways to suppress the error
  class Suppression {
    String notNullIsBAD; // BAD: need to initialize it

    @Nonnull String nonnullIsBAD; // BAD: explicit annotation does not make it better

    @NonNull String nonNullIsBAD; // BAD: explicit annotation does not make it better

    @Nullable String nullableIsOK; // OK: will be init with null

    @SuppressFieldNotInitialized String suppressAnnotationIsOK; // OK: explicitly suppressed

    @SuppressLint("eradicate-field-not-initialized")
    String suppressLintIsOK; // OK: explicitly suppressed on lint level

    @SuppressLint("some-irrelevant-linter")
    String suppressWrongLintIsBAD; // BAD: this suppression is irrelevant

    @Inject String injectIsOK; // Means: assume it will be initialized via dependency injection

    @Bind String bindIsOK; // Means: assume it will be initialized, and ignore null assignment

    // Means: assume it will be initialized, and ignore null assignment
    @SuppressViewNullability String suppressViewNullabilityIsOK;

    Suppression() {}

    // Ensure that some suppressions suppress only field not initialized
    // and nothing else, but some suppress setting to null as well.
    void testNullifyFields() {
      bindIsOK = null; // OK: the framework could write null into the field
      suppressViewNullabilityIsOK = null; // OK: the framework could write null into the field
      nonnullIsBAD = null; // BAD: explicit nonnull annotation does not allow nullifying
      nonNullIsBAD = null; // BAD: explicit nonnull annotation does not allow nullifying
      injectIsOK = null; // BAD: inject suppressed only initialization issues
      suppressAnnotationIsOK = null; // BAD: only initialization issue was suppressed
      suppressLintIsOK = null; // BAD: only initialization issue was suppressed
    }
  }

  class OnlyRead {
    Object o;

    OnlyRead() {
      Object x = o; // BAD: we merely read this variable, but forgot to initialize
    }
  }

  class WriteItselfIsBAD {
    Object ok;
    Object bad;

    WriteItselfIsBAD() {
      ok = "";
      bad = bad; // BAD: Can not initialize with itself
    }
  }

  class InitializationOrder {
    Object o1;
    Object o2;

    InitializationOrder(int a) {
      o1 = o2; // BAD: not initialized
      o2 = new Object();
    }

    InitializationOrder(double a) {
      o1 = new Object(); // OK
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

  class ShouldInitializeInAllBranches {
    Object f1;
    Object f2;
    Object f3;
    Object f4;
    Object f5;

    public ShouldInitializeInAllBranches(int a) {
      f4 = new Object();
      Object f5 = new Object(); // BAD: shadowing; not an initialization
      if (a == 42) {
        f1 = new Object();
        f2 = new Object();
      } else {
        f3 = new Object();
        if (a == 43) {
          f1 = new Object();
          // BAD: f2 is not initialized in this branch
        } else {
          f1 = new Object();
          f2 = new Object();
        }
      }
    }
  }

  class InitIfNull {
    Object good;
    Object shouldBeGood_FIXME;

    public InitIfNull() {
      if (good == null) {
        good = new Object();
        // bad is considered to be initialized not in all branches.
        // (which is bit weird: we know that good is always null by default)
        // TODO(T53537343) teach nullsafe that all fields are initially null in constructor
        shouldBeGood_FIXME = new Object();
      }
    }
  }

  class InitCircular {
    String bad;
    String stillBad;
    String good;

    String knownNotNull = "";

    InitCircular() {
      String tmp = bad;
      bad = tmp; // BAD: circular initialization
      stillBad = bad; // BAD: try to initialize from circularly initalized var

      String tmp2 = knownNotNull;
      good = tmp2; // OK
    }
  }

  class InitWithOtherClass {
    class OtherClass {
      String nonNull = "";
      @Nullable String nullable = "";
    }

    String bad;
    String good;

    InitWithOtherClass(OtherClass x) {
      bad = x.nullable; // BAD: might be null
      good = x.nonNull; // OK: we know can not be null
    }
  }

  // Check that Infer does not confuse things
  // in copy constuctors.
  class InitWithTheSameClass {
    String good;
    String bad;

    InitWithTheSameClass(InitWithTheSameClass x) {
      good = x.good; // OK: this is not a circular initialization
      bad = bad; // BAD: this is a circular initialization
    }
  }
}

/**
 * There is a predefined list of classes which have known methods that act like initializers. If a
 * class extends such special class and initializes a field in such whitelisted method, we don't
 * require initializing this field in constructor. (NOTE: To do the same in non whitelisted class
 * one can use @Initializer annotation instead).
 */
class TestKnownInitializers {

  // nullsafe knows that Activity.onCreate is a special initilizer.
  class KnownInitializers extends Activity {

    String initInConstructorIsOK;
    String initInSpecialMethodIsOK;
    String initInUnknownMethodIsBAD;

    KnownInitializers() {
      initInConstructorIsOK = "";
    }

    // onCreate is a special method
    protected void onCreate(Bundle bundle) {
      initInSpecialMethodIsOK = "";
    }

    protected void someOtherMethod(Bundle bundle) {
      // BAD: This method is unknown (and does not have @Initializer annotation either).
      initInUnknownMethodIsBAD = "";
    }
  }

  abstract class FakeActivity {
    abstract void onCreate(Bundle bundle);
  }

  class SimplyOnCreateWontDoATrick extends FakeActivity {
    String initInUnknownMethodIsBAD;

    // Though we use onCreate method, the class does not extend one of the known
    // classes that are known to have such a property, so it does not count.
    protected void onCreate(Bundle bundle) {
      // BAD: This method is unknown (and does not have @Initializer annotation either).
      initInUnknownMethodIsBAD = "";
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
