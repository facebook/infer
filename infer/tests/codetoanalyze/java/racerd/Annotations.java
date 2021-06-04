/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import android.support.annotation.UiThread;
import com.facebook.infer.annotation.Functional;
import com.facebook.infer.annotation.Initializer;
import com.facebook.infer.annotation.ReturnsOwnership;
import com.facebook.infer.annotation.SynchronizedCollection;
import com.facebook.infer.annotation.ThreadConfined;
import com.facebook.infer.annotation.ThreadSafe;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/** tests for classes and method annotations that are meaningful w.r.t thread-safety */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnBind {}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnEvent {}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnMount {}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnUnbind {}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnUnmount {}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface MyThreadSafeAlias1 {}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface MyThreadSafeAlias2 {}

@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.CLASS)
@interface InjectProp {}

interface Interface {

  @Functional
  Object functionalMethod();

  @ReturnsOwnership
  Obj returnsOwnershipMethod();
}

@ThreadSafe(enableChecks = false)
class AssumedThreadSafe {

  Object field;

  public void writeOk() {
    this.field = new Object();
  }
}

// this annotation is defined as an alias for @ThreadSafe in .inferconfig
class ThreadSafeAlias {
  Object field;

  @MyThreadSafeAlias1
  void threadSafeAliasBad1() {
    this.field = new Object();
  }

  @MyThreadSafeAlias2
  void threadSafeAliasBad2() {
    this.field = new Object();
  }
}

@ThreadSafe
class Annotations implements Interface {
  Object f;
  boolean b;

  @UiThread
  public void setF(Object newF) {
    this.f = newF; // shouldn't report here
  }

  public void callSetFOnMethodOk(Annotations obj) {
    obj.setF(new Object()); // or here
  }

  public void mutateOffUiThreadBad() {
    this.f = new Object();
  }

  // anything annotated with OnEvent is modeled as running on the UI thread, should not warn
  @OnEvent
  public void onClick() {
    this.f = new Object();
  }

  Confined con;

  public void confinedCallerOk() {
    con.foo();
  }

  public void writeFieldOfConfinedClassOk() {
    con.x = 7;
  }

  @ThreadConfined(ThreadConfined.UI)
  class Confined {
    Integer x;

    void foo() {
      x = 22;
    }
  }

  @ThreadConfined(ThreadConfined.ANY)
  Obj encapsulatedField;

  public void mutateConfinedFieldDirectlyOk() {
    this.encapsulatedField = new Obj();
  }

  public static void mutateConfinedFieldIndirectlyOk(Annotations a) {
    a.encapsulatedField = new Obj();
  }

  public void mutateSubfieldOfConfinedBad() {
    this.encapsulatedField.f = new Object();
  }

  Integer zz;

  @ThreadConfined("some_custom_string")
  public void threadConfinedMethodOk() {
    this.f = new Object();
    zz = 22;
  }

  public void read_from_non_confined_method_Bad() {
    Integer i;
    i = zz;
  }

  /* Like in RaceWithMainThread.java with assertMainThread() */
  void conditional1_ok(boolean b) {
    if (b) {
      write_on_main_thread_ok();
    }
  }

  Integer ii;

  @ThreadConfined(ThreadConfined.UI)
  void write_on_main_thread_ok() {
    ii = 22;
  }

  void conditional2_bad(boolean b) {
    if (b) {
      write_on_main_thread_ok();
    } else {
      ii = 99; // this might or might not run on the main thread; warn
    }
  }

  @OnBind
  public void onBindMethodOk() {
    this.f = new Object();
  }

  public void read_off_UI_thread_Bad() {
    Object o = f;
  }

  @OnMount
  public void onMountMethodOk() {
    this.f = new Object();
  }

  @OnUnmount
  public void onUnmountMethodOk() {
    this.f = new Object();
  }

  @OnUnbind
  public void onUnbindMethodOk() {
    this.f = new Object();
  }

  @ThreadSafe(enableChecks = false)
  public void assumeThreadSafeOk() {
    this.f = new Object();
  }

  @Functional
  native Object returnFunctional1();

  @Functional
  Object returnFunctional2() {
    return null;
  }
  // marked @Functional in interface
  @Override
  public Object functionalMethod() {
    return null;
  }

  Object mAssignToFunctional;

  public Object functionalOk1() {
    if (mAssignToFunctional == null) {
      mAssignToFunctional = returnFunctional1();
    }
    return mAssignToFunctional;
  }

  public Object functionalOk2() {
    if (mAssignToFunctional == null) {
      mAssignToFunctional = returnFunctional2();
    }
    return mAssignToFunctional;
  }

  public Object functionalOk3() {
    if (mAssignToFunctional == null) {
      mAssignToFunctional = functionalMethod();
    }
    return mAssignToFunctional;
  }

  @Functional
  native double returnDouble();

  @Functional
  native long returnLong();

  double mDouble;
  long mLong;

  int mInt1;
  int mInt2;

  public int functionalAcrossUnboxingAndCast1Ok() {
    if (b) {
      mInt1 = (int) returnDouble();
    }
    return 0;
  }

  public int functionalAcrossUnboxingAndCast2Ok() {
    if (b) {
      mInt2 = (int) returnLong();
    }
    return 0;
  }

  // writes to doubles are not atomic on all platforms, so this is not a benign race
  public double functionalDoubleBad() {
    if (b) {
      mDouble = returnDouble();
    }
    return 0.0;
  }

  // writes to longs are not atomic on all platforms, so this is not a benign race
  public long functionaLongBad() {
    if (b) {
      mLong = returnLong();
    }
    return 2;
  }

  Boolean mBoxedBool;

  @Functional
  native boolean returnBool();

  public boolean functionalAcrossBoxingOk() {
    if (b) {
      mBoxedBool = returnBool();
    }
    return b;
  }

  boolean mBool;

  @Functional
  native Boolean returnBoxedBool();

  boolean mBool2;

  public boolean FP_functionalAcrossUnboxingOk() {
    if (b) {
      mBool2 = returnBoxedBool();
    }
    return b;
  }

  Long mBoxedLong;

  @Functional
  native Long returnBoxedLong();

  public int functionalBoxedLongOk() {
    if (b) {
      mBoxedLong = returnBoxedLong();
    }
    return 22;
  }

  long mLong2;

  public int functionalAcrossUnboxingLongBad() {
    if (b) {
      mLong2 = returnBoxedLong();
    }
    return 2;
  }

  long mBoxedLong2;

  public int FP_functionalAcrossBoxingLongOk() {
    if (b) {
      mBoxedLong2 = returnLong();
    }
    return 2;
  }

  public boolean propagateFunctional() {
    return returnBool();
  }

  // show that we can handle indirect returns of procedures marked @Functional
  public void propagateFunctionalOk() {
    boolean returnedFunctional = propagateFunctional();
    mBool = returnedFunctional;
  }

  @Functional
  native int returnInt();

  int mInt;

  public void functionalAcrossLogicalOpsOk() {
    boolean functionalBool = returnBool();
    int functionalInt = returnInt();
    boolean propagated = functionalBool && true || 2 < returnInt() && 3 == functionalInt;
    mBool = propagated;
  }

  public void functionalAcrossArithmeticOpsOk() {
    int functional = returnInt();
    int propagated = functional + 1 - returnInt() * 7 % 2;
    mInt = functional;
  }

  native int returnNonFunctionalInt();

  public void functionalAndNonfunctionalBad() {
    mInt = returnNonFunctionalInt() + returnInt();
  }

  @ReturnsOwnership
  native Obj returnsOwned();

  @Override
  public native Obj returnsOwnershipMethod(); // marked @ReturnsOwnership in interface

  void mutateAnnotatedOwnedOk() {
    Obj owned = returnsOwned();
    owned.f = new Object();
  }

  void mutateAnnotatedOverrideOwnedOk() {
    Obj owned = returnsOwnershipMethod();
    owned.f = new Object();
  }

  public void writeToAssumedThreadSafeClassOk(AssumedThreadSafe c) {
    c.writeOk();
  }

  @SynchronizedCollection
  private final Map<Object, Object> mSynchronizedMap = Collections.synchronizedMap(new HashMap());

  public void synchronizedMapOk1() {
    mSynchronizedMap.put(new Object(), new Object());
  }

  public void synchronizedMapOk2(Annotations a) {
    a.mSynchronizedMap.put(new Object(), new Object());
  }

  public void injectPropOk(@InjectProp Obj o) {
    o.f = 7;
  }

  Object onlyUpdatedInInitializer;

  @Initializer
  public void setOnlyUpdatedInInitializerOk(Object o) {
    onlyUpdatedInInitializer = o;
  }

  public synchronized Object getOnlyUpdatedInInitializerOk() {
    return onlyUpdatedInInitializer;
  }
}

@UiThread
@ThreadSafe
class AllMethodsOnUiThread {
  int f;

  void fooOk() {
    f = 5;
  }

  int bar() {
    return f;
  }
}

class ExtendsClassOnUiThread extends AllMethodsOnUiThread {
  @Override
  void fooOk() {
    f = 9;
  }

  @Override
  int bar() {
    return super.bar();
  }
}

// NOT All annotations that start with "On" are on the main thread
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@interface OnXYZ {}

@ThreadSafe
class WeirdAnnotation {
  int f;

  @OnXYZ
  void fooBad() {
    f = 0;
  }
}
