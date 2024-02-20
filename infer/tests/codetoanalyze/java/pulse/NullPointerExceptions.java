/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import android.annotation.SuppressLint;
import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.text.TextUtils;
import com.facebook.infer.annotation.Assertions;
import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.HashMap;
import java.util.concurrent.locks.Lock;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public class NullPointerExceptions {

  class A {
    int x;

    int thisNotNullOk() {
      if (this == null) {
        A a = null;
        return a.x;
      }
      return 0;
    }

    int thisNotNullBad() {
      if (this != null) {
        A a = null;
        return a.x;
      }
      return 0;
    }

    public void method() {}
  }

  // npe local with field
  public int nullPointerException() {
    A a = null;
    return a.x;
  }

  public A canReturnNullObject(boolean ok) {
    A a = new A();
    if (ok) return a;
    else return null;
  }

  public static void expectNotNullObjectParameter(A a) {
    a.method();
  }

  public static void expectNotNullArrayParameter(A[] array) {
    array.clone();
  }

  // npe with branching, interprocedural
  public int nullPointerExceptionInterProc() {
    A a = canReturnNullObject(false);
    return a.x;
  }

  // npe with exception handling
  public int nullPointerExceptionWithExceptionHandling(boolean ok) {
    A a = null;
    try {
      throw new Exception();
    } catch (Exception e) {
      return a.x;
    }
  }

  class B {
    A a;

    public B() {}

    public B(A a) {
      this.a = a;
    }

    void test() {}
  }

  public static int nullPointerExceptionWithArray() {
    A[] array = new A[] {null};
    A t = array[0];
    return t.x;
  }

  // npe with a chain of fields
  class C {
    B b;
  }

  public int FN_nullPointerExceptionWithNullByDefaultBad(C c) {
    return (new B()).a.x;
  }

  public int nullPointerExceptionWithExplicitNullInitBad(C c) {
    return (new B(null)).a.x;
  }

  public int FN_nullPointerExceptionWithAChainOfFields(C c) {
    c.b = new B();
    return c.b.a.x;
  }

  // npe with a null object parameter
  public static void nullPointerExceptionWithNullObjectParameter() {
    expectNotNullObjectParameter(null);
  }

  // npe with a null array parameter
  public static void nullPointerExceptionWithNullArrayParameter() {
    expectNotNullArrayParameter(null);
  }

  public static void nullPointerExceptionFromFaillingResourceConstructor() throws IOException {
    FileInputStream fis = null;
    try {
      fis = new FileInputStream(new File("whatever.txt"));
    } catch (IOException e) {
    } finally {
      fis.close();
    }
  }

  public static void nullPointerExceptionFromFailingFileOutputStreamConstructor()
      throws IOException {
    FileOutputStream fos = null;
    try {
      fos = new FileOutputStream(new File("whatever.txt"));
    } catch (IOException e) {
    } finally {
      fos.close();
    }
  }

  int x;

  public void nullPointerExceptionFromNotKnowingThatThisIsNotNull() {
    if (this == null) {}
    this.x = 4;
  }

  public <T> T id_generics(T o) {
    o.toString();
    return o;
  }

  public A frame(A x) {
    return id_generics(x);
  }

  public void nullPointerExceptionUnlessFrameFails() {
    String s = null;
    Object a = frame(new A());
    if (a instanceof A) {
      s.length();
    }
  }

  class D {
    int x;
  }

  public int preconditionCheckStateTest(D d) {
    Preconditions.checkState(d != null);
    return d.x;
  }

  public void genericMethodSomewhereCheckingForNull(String s) {
    if (s == null) {}
  }

  public void noNullPointerExceptionAfterSkipFunction() {
    String t = new String("Hello!");
    String s = t.toString();
    genericMethodSomewhereCheckingForNull(s);
    s.length();
  }

  String hashmapNPE(HashMap h, Object o) {
    return (h.get(o).toString());
  }

  String NPEhashmapProtectedByContainsKey(HashMap h, Object o) {
    if (h.containsKey(o)) {
      return (h.get(o).toString());
    }
    return "aa";
  }

  int NPEvalueOfFromHashmapBad(HashMap<Integer, Integer> h, int position) {
    return h.get(position);
  }

  Integer NPEvalueOfFromHashmapGood(HashMap<Integer, Integer> h, int position) {
    return h.get(position);
  }

  void nullPointerExceptionInArrayLengthLoop(Object[] arr) {
    for (int i = 0; i < arr.length; i++) {
      Object x = null;
      x.toString();
    }
  }

  Context mContext;
  ContentResolver mContentResolver;

  public void cursorFromContentResolverNPE(String customClause) {
    String[] projection = {"COUNT(*)"};
    String selectionClause = selectionClause = customClause;
    Cursor cursor =
        mContext.getContentResolver().query(null, projection, selectionClause, null, null);
    cursor.close();
  }

  public int cursorQueryShouldNotReturnNull(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    try {
      return cursor.getCount();
    } finally {
      cursor.close();
    }
  }

  Object[] arr = new Object[1];

  Object arrayReadShouldNotCauseSymexMemoryError(int i) {
    arr[i].toString();
    return null;
  }

  void nullPointerExceptionCallArrayReadMethod() {
    arr[0] = new Object();
    arrayReadShouldNotCauseSymexMemoryError(0).toString();
  }

  public void FP_sinkWithNeverNullSource() {
    NeverNullSource source = new NeverNullSource();
    T t = source.get();
    t.f();
  }

  public void FP_otherSinkWithNeverNullSource() {
    SomeLibrary source = new SomeLibrary();
    T t = source.get();
    t.f();
  }

  private @Nullable Object mFld;

  void FN_nullableFieldNPE() {
    mFld.toString();
  }

  void guardedNullableFieldDeref() {
    if (mFld != null) mFld.toString();
  }

  void allocNullableFieldDeref() {
    mFld = new Object();
    mFld.toString();
  }

  void nullableParamNPE(@Nullable Object param) {
    param.toString();
  }

  void guardedNullableParamDeref(@Nullable Object param) {
    if (param != null) param.toString();
  }

  void allocNullableParamDeref(@Nullable Object param) {
    param = new Object();
    param.toString();
  }

  native boolean test1();

  native boolean test2();

  Object getObj() {
    if (test1()) {
      return new Object();
    } else {
      return null;
    }
  }

  Boolean getBool() {
    if (test2()) {
      return new Boolean(true);
    } else {
      return null;
    }
  }

  void derefGetterAfterCheckShouldNotCauseNPE() {
    if (getObj() != null) {
      getObj().toString();
    }
  }

  void derefBoxedGetterAfterCheckShouldNotCauseNPE() {
    boolean b = getBool() != null && getBool();
  }

  static void derefNonThisGetterAfterCheckShouldNotCauseNPE() {
    NullPointerExceptions c = new NullPointerExceptions();
    if (c.getObj() != null) {
      c.getObj().toString();
    }
  }

  // latent because we don't know if test1() can return "true"
  // arguably should be reported?
  void badCheckShouldCauseNPE_latent() {
    if (getBool() != null) {
      getObj().toString();
    }
  }

  void nullPointerExceptionArrayLength() {
    Object[] arr = null;
    int i = arr.length;
  }

  class $$Class$Name$With$Dollars {
    void npeWithDollars() {
      String s = null;
      int n = s.length();
    }
  }

  void nullableNonNullStringAfterTextUtilsIsEmptyCheckShouldNotCauseNPE(@Nullable String str) {
    if (!TextUtils.isEmpty(str)) {
      str.length();
    }
  }

  void someNPEAfterResourceLeak() {
    T t = CloseableAsResourceExample.sourceOfNullWithResourceLeak();
    t.f();
  }

  private Object mOkObj = new Object();

  public void nullableParamReassign1(@Nullable Object o) {
    if (o == null) {
      o = mOkObj;
    }
    o.toString();
  }

  public void nullableParamReassign2(@Nullable Object o, Object okObj) {
    if (o == null) {
      o = okObj;
    }
    o.toString();
  }

  private @Nullable Object mNullableField;

  public void nullableFieldReassign1() {
    if (mNullableField == null) {
      mNullableField = mOkObj;
    }
    mNullableField.toString();
  }

  public void nullableFieldReassign2(Object okObj) {
    if (mNullableField == null) {
      mNullableField = okObj;
    }
    mNullableField.toString();
  }

  public void nullableFieldReassign3(Object param) {
    mNullableField = param;
    mNullableField.toString();
  }

  public Object nullableGetter() {
    return mNullableField;
  }

  public void FN_derefNullableGetter() {
    Object o = nullableGetter();
    o.toString();
  }

  public @Nullable Object nullableRet(boolean b) {
    if (b) {
      return null;
    }
    return new Object();
  }

  public void derefNullableRet(boolean b) {
    Object ret = nullableRet(b);
    ret.toString();
  }

  public void derefNullableRetOK(boolean b) {
    Object ret = nullableRet(b);
    if (ret != null) {
      ret.toString();
    }
  }

  public native @Nullable Object undefNullableRet();

  public void derefUndefNullableRet() {
    Object ret = undefNullableRet();
    ret.toString();
  }

  public void derefUndefNullableRetOK() {
    Object ret = undefNullableRet();
    if (ret != null) {
      ret.toString();
    }
  }

  void assumeUndefNullableIdempotentOk() {
    if (undefNullableRet() != null) {
      undefNullableRet().toString();
    }
  }

  public Object undefNullableWrapper() {
    return undefNullableRet();
  }

  public void derefUndefNullableRetWrapper() {
    undefNullableWrapper().toString();
  }

  private int returnsThreeOnlyIfRetNotNull(Object obj) {
    if (obj == null) {
      return 2;
    }
    return 3;
  }

  public void testNullablePrecision() {
    Object ret = undefNullableRet();
    if (returnsThreeOnlyIfRetNotNull(ret) == 3) {
      ret.toString(); // shouldn't warn here
    }
  }

  public @Nullable String FN_testSystemGetPropertyArgument() {
    String s = System.getProperty(null);
    return s;
  }

  public void FN_testSystemGetPropertyReturn() {
    String s = System.getProperty("");
    int n = s.length();
  }

  Object retUndefined() {
    return "".toString(); // toString is a skip function
  }

  Object derefUndefinedCallee() {
    // if retUndefined() is handled incorrectly, we get a symexec_memory_error here
    retUndefined().toString();
    return null;
  }

  void derefNull() {
    // should be NPE, but will not be reported if we handled retUndefined() incorrectly
    derefUndefinedCallee().toString();
  }

  @SuppressLint("NULL_DEREFERENCE")
  void shouldNotReportNPE() {
    Object o = null;
    o.toString();
  }

  void shouldNotReportOnSkippedSource() {
    Object o = SkippedSourceFile.createdBySkippedFile();
    o.toString();
  }

  int nullListFiles(String pathname) {
    File dir = new File(pathname);
    File[] files = dir.listFiles();
    return files.length; // expect possible NullPointerException as files == null is possible
  }

  native Object unknownFunc();

  void nullDerefernceReturnOfSkippedFunctionBad() {
    Object object = unknownFunc();
    if (object == null) {
      object.toString();
    }
  }

  native @Nonnull Object doesNotReturnNull();

  void noNPEWhenCallingSkippedNonnullAnnotatedMethodGood() {
    Object object = doesNotReturnNull();
    if (object == null) {
      object.toString();
    }
  }

  Object callUnknownFunc() {
    return unknownFunc();
  }

  void dontReportOnNullableDirectReassignmentToUnknown(@Nullable Object o) {
    o = unknownFunc();
    o.toString();
  }

  void dontReportOnNullableIndirectReassignmentToUnknown(@Nullable Object o) {
    o = callUnknownFunc();
    o.toString();
  }

  @Nullable
  Object wrapUnknownFuncWithNullable() {
    return unknownFunc();
  }

  void deferenceNullableMethodCallingSkippedMethodBad() {
    wrapUnknownFuncWithNullable().toString();
  }

  String nullTryLock(FileChannel chan) throws IOException {
    FileLock lock = chan.tryLock();
    return lock.toString(); // expect possible NullPointerException as lock == null is possible
  }

  String tryLockThrows(FileChannel chan) {
    try {
      FileLock lock = chan.tryLock();
      return (lock != null ? lock.toString() : "");
    } catch (IOException e) {
      Object o = null;
      return o.toString(); // expect NullPointerException as tryLock can throw
    }
  }

  class L {
    L next;
  }

  Object returnsNullAfterLoopOnList(L l) {
    while (l != null) {
      l = l.next;
    }
    return null;
  }

  void dereferenceAfterLoopOnList(L l) {
    Object obj = returnsNullAfterLoopOnList(l);
    obj.toString();
  }

  void dereferenceAfterUnlock1(Lock l) {
    l.unlock();
    String s = l.toString();
    s = null;
    s.toString(); // Expect NPE here
  }

  void dereferenceAfterUnlock2(Lock l) {
    synchronized (l) {
      String b = null;
    }
    String s = l.toString();
    s = null;
    s.toString(); // Expect NPE here
  }

  void optionalNPE(Optional<Object> o) {
    o.orNull().toString();
  }

  void FP_stringConstantEqualsTrueNotNPE() {
    final String c1 = "Test string!";
    final String c2 = "Test string!";
    String s = null;
    if (c1.equals(c1)) {
      s = "safe";
    }
    s.toString(); // No NPE
    s = null;
    if (c1.equals(c2)) {
      s = "safe";
    }
    s.toString(); // No NPE
  }

  void stringConstantEqualsFalseNotNPE_FP() {
    // This won't actually cause an NPE, but our current model for String.equals
    // returns boolean_undefined for all cases other than String constant
    // equality. Consider handling constant inequality in the future.
    final String c1 = "Test string 1";
    final String c2 = "Test string 2";
    String s = null;
    if (!c1.equals(c2)) {
      s = "safe";
    }
    s.toString(); // No NPE
  }

  String getString2() {
    return "string 2";
  }

  void stringVarEqualsFalseNPE() {
    final String c1 = "Test string 1";
    String c2 = "Test " + getString2();
    String s = null;
    if (!c1.equals(c2)) {
      s.toString(); // NPE
    }
  }

  String assertParameterNotNullableOk(@Nullable Object object) {
    return Assertions.assertNotNull(object).toString();
  }

  interface I {
    @Nullable Object mObject = null;
  }

  class E implements I {

    void FN_dereferenceNullableInterfaceFieldBad() {
      mObject.toString();
    }
  }

  Object getObject() {
    return null;
  }

  void FN_addNullToImmutableListBuilderBad() {
    ImmutableList.Builder<Object> listBuilder = ImmutableList.builder();
    listBuilder.add(getObject());
  }

  void incr_deref(A a1, A a2) {
    a1.x++;
    a2.x++;
  }

  void call_incr_deref_with_alias_bad() {
    A a = new A();
    a.x = 0;
    incr_deref(a, a);
    if (a.x == 2) {
      a = null;
    }
    a.x = 0;
  }

  void call_incr_deref_with_alias_Ok() {
    A a = new A();
    a.x = 0;
    incr_deref(a, a);
    if (a.x != 2) {
      a = null;
    }
    a.x = 0;
  }

  // An other example wich require alias specialization, but with interfering calls
  // with and without alias
  void incr_deref2(A a1, A a2) {
    a1.x++;
    a2.x++;
  }

  void call_incr_deref2_bad() {
    A a = new A();
    A a1 = new A();
    A a2 = new A();
    a.x = 0;
    a1.x = 0;
    a2.x = 0;
    incr_deref2(a, a);
    incr_deref2(a1, a2);
    if (a1.x == 1 && a2.x == 1 && a.x == 2) {
      a1 = null;
    }
    a1.x = 0;
  }

  void call_incr_deref2_Ok() {
    A a = new A();
    A a1 = new A();
    A a2 = new A();
    a.x = 0;
    a1.x = 0;
    a2.x = 0;
    incr_deref2(a, a);
    incr_deref2(a1, a2);
    if (!(a1.x == 1 && a2.x == 1 && a.x == 2)) {
      a1 = null;
    }
    a1.x = 0;
  }

  // An other example wich require multiple alias specializations
  void incr_deref3(A a1, A a2, A a3) {
    a1.x++;
  }

  void call_incr_deref3_bad() {
    A a1 = new A();
    A a2 = new A();
    A a3 = new A();
    incr_deref3(null, null, null);
  }

  interface AFunction {
    void run(A a);
  }

  // need combination of alias specialization and dynamic type specialization
  void test_capture_alias_bad() {
    A a = new A();
    a.x = 0;
    AFunction incr_deref =
        (a2) -> {
          a.x++;
          a2.x++;
        };
    incr_deref.run(a);
    A b = a;
    if (a.x == 2) {
      b = null;
    }
    b.x = 0;
  }

  void test_capture_alias_good() {
    A a = new A();
    a.x = 0;
    AFunction incr_deref =
        (a2) -> {
          a.x++;
          a2.x++;
        };
    incr_deref.run(a);
    A b = a;
    if (a.x != 2) {
      b = null;
    }
    b.x = 0;
  }
}
