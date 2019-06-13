/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
public class Escape {

  private Obj mField;
  private static Obj sGlobal;

  // this can race with unsafe writes to mField.f
  public synchronized Object racyRead1() {
    return mField.f;
  }

  // this can race with unsafe writes to mField.f
  public synchronized Object racyRead2() {
    return sGlobal.f;
  }

  public void FN_fieldEscapeBad() {
    Obj o = new Obj();
    synchronized (this) {
      mField = o;
    }
    o.f = new Object(); // not safe
  }

  public void FN_globalEscapeBad() {
    Obj o = new Obj();
    synchronized (Escape.class) {
      sGlobal = o;
    }
    o.f = new Object(); // not safe
  }

  public synchronized void escapeInCallee(Obj o) {
    mField = o;
  }

  public void FN_escapeInCalleeBad() {
    Obj o = new Obj();
    escapeInCallee(o);
    o.f = new Object();
  }

  public void aliasOk() {
    Obj o = new Obj(); // though there's two pointers to this address, neither escapes
    Obj alias = o;
    o.f = null;
    alias.f = null;
  }

  public void nonAliasReadOk() {
    Obj o = new Obj();
    String s = o + "a";
    o.f = null; // ok
  }

  public void FN_escapeViaAliasBad1() {
    Obj o = new Obj();
    Obj alias = o;
    escapeInCallee(alias);
    o.f = null; // bad
    alias.f = null; // bad
  }

  public void FN_escapeViaAliasBad2() {
    Obj o = new Obj();
    Obj alias = o;
    escapeInCallee(o);
    o.f = null; // bad
    alias.f = null; // bad
  }

  public Obj id(Obj o) {
    return o;
  }

  public void FN_aliasViaReturnBad1() {
    Obj o = new Obj();
    Obj alias = id(o);
    escapeInCallee(alias);
    o.f = null; // bad
    alias.f = null; // bad
  }

  public void FN_aliasViaReturnBad2() {
    Obj o = new Obj();
    Obj alias = id(o);
    escapeInCallee(o);
    o.f = null; // bad
    alias.f = null; // bad
  }

  private void twoParamsOneEscapes(Obj o1, Obj o2) {
    synchronized (Escape.class) {
      sGlobal = o1;
    }
    o1.f = null; // only safe if o1/o2 not aliased
  }

  public void FN_aliasedParamsBad() {
    Obj o = new Obj();
    twoParamsOneEscapes(o, o); // should report racy write in callee
    o.f = null; // bad
  }

  public void nonAliasedParamsOk() {
    Obj o1 = new Obj();
    Obj o2 = new Obj();
    twoParamsOneEscapes(o1, o2);
    o2.f = null; // ok
  }
}

@ThreadSafe
class Leaky {

  Leaky mLeak;
  Object mField;
  Object sGlobal;

  public Leaky() {
    mLeak = this;
  }

  public void FN_leakyConstructorBad() {
    Leaky l = new Leaky();
    synchronized (Leaky.class) {
      sGlobal = l.mLeak; // oops, this leaks l
    }
    l.mField = 1; // bad
  }
}
