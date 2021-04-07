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

  public int nullPointerExceptionWithAChainOfFields(C c) {
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

  public void FP_noNullPointerExceptionAfterSkipFunction() {
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

  public void sinkWithNeverNullSource() {
    NeverNullSource source = new NeverNullSource();
    T t = source.get();
    t.f();
  }

  public void otherSinkWithNeverNullSource() {
    SomeLibrary source = new SomeLibrary();
    T t = source.get();
    t.f();
  }

  private @Nullable Object mFld;

  void nullableFieldNPE() {
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

  native boolean test();

  Object getObj() {
    if (test()) {
      return new Object();
    } else {
      return null;
    }
  }

  Boolean getBool() {
    if (test()) {
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

  void badCheckShouldCauseNPE() {
    if (getBool() != null) getObj().toString();
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
    T t = CloseableAsResourceExample.sourceOfNullWithResourceLeakBad();
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

  public void derefNullableGetter() {
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

  public @Nullable String testSystemGetPropertyArgument() {
    String s = System.getProperty(null);
    return s;
  }

  public void testSystemGetPropertyReturn() {
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

  void stringConstantEqualsTrueNotNPE() {
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

    void dereferenceNullableInterfaceFieldBad() {
      mObject.toString();
    }
  }

  Object getObject() {
    return null;
  }

  void addNullToImmutableListBuilderBad() {
    ImmutableList.Builder<Object> listBuilder = ImmutableList.builder();
    listBuilder.add(getObject());
  }
}
