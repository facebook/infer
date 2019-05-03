/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import java.lang.ref.WeakReference;

import android.content.Context;
import android.app.Activity;
import android.os.Bundle;

public class ContextLeaks extends Activity {

  private static Context bob;
  static Object sFld;

  void directLeak() {
    sFld = this;
  }

  void bobLeak(){
    bob = this;
  }

  public void whatLeak(){
    directLeak();
  }

  public void leakThenFix() {
    sFld = this;
    sFld = null;
  }

  public void nonActivityNoLeak() {
    sFld = new Object();
  }

  static class Obj {
    public Object f;
  }

  public void indirectLeak() {
    Obj o = new Obj();
    o.f = this;
    sFld = o;
  }

  public void indirectLeakThenFix() {
    Obj o = new Obj();
    o.f = this;
    sFld = o;
    o.f = null;
  }

  class NonStaticInner {
  }

  public void nonStaticInnerClassLeak() {
    sFld = new NonStaticInner();
  }

  public void nonStaticInnerClassLeakThenFix() {
    sFld = new NonStaticInner();
    sFld = null;
  }

  private Object o;

  public void leakAfterInstanceFieldWrite() {
    this.o = new Object();
    sFld = this;
  }

  public static class Singleton {

    private static Singleton instance;
    private Context context;

    private Singleton(Context context) {
      this.context = context;
    }

    public static Singleton getInstance(Context context) {
      if (instance == null) {
        instance = new Singleton(context);
      }
      return instance;
    }
  }

  public Singleton singletonLeak() {
    return Singleton.getInstance(this);
  }

  public Singleton singletonNoLeak() {
    return Singleton.getInstance(this.getApplicationContext());
  }

  // testing that we don't report on static field -> ... -> Context paths broken by weak refs
  static WeakReference<Context> sDirectWeakReference;

  static WeakReference<Obj> sIndirectWeakReference1;

  static Obj sIndirectWeakReference2;

  // sDirectWeakReference |-> WeakReference.referent |-> Context
  public void directWeakReferenceOk() {
    sDirectWeakReference = new WeakReference(this);
  }

  // sIndirectWeakReference1 |-> WeakReference.referent |-> Obj.f |-> Context
  public void indirectWeakReferenceOk1() {
    Obj obj = new Obj();
    obj.f = this;
    sIndirectWeakReference1 = new WeakReference(obj);
  }

  // sIndirectWeakReference2.|-> Obj.f |-> WeakReference.referent |-> Context
  public void indirectWeakReferenceOk2() {
    Obj obj = new Obj();
    obj.f = new WeakReference(this);
    sIndirectWeakReference2 = obj;
  }

}
