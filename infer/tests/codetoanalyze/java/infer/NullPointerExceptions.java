/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package codetoanalyze.java.infer;

import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.text.TextUtils;

import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;

import javax.annotation.Nullable;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;

public class NullPointerExceptions {

  class A {
    int x;

    public void method() {
    }
  }

  // npe local with field
  public int nullPointerException() {
    A a = null;
    return a.x;
  }

  public A canReturnNullObject(boolean ok) {
    A a = new A();
    if (ok)
      return a;
    else
      return null;
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

    void test() {
    }
  }


  public static int nullPointerExceptionWithArray() {
    A[] array = new A[]{null};
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
    if (this == null) {
    }
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
    if (s == null) {
    }
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

  int NPEvalueOfFromHashmapBad(HashMap<Integer,Integer> h, int position) {
    return h.get(position);
  }

  Integer NPEvalueOfFromHashmapGood(HashMap<Integer,Integer> h, int position) {
    return h.get(position);
  }

  static void ReturnedValueOfImmutableListOf() {
    ImmutableList<Object> l = ImmutableList.of();
    if (l == null) {
      l.toString();
    }
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
    Cursor cursor = mContext.getContentResolver().query(
                      null,
                      projection,
                      selectionClause,
                      null,
                      null);
    cursor.close();
  }

  public int cursorQueryShouldNotReturnNull(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query(
              "events", null, null, null, null, null, null);
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
    if(!TextUtils.isEmpty(str))
      str.length();
  }

}
