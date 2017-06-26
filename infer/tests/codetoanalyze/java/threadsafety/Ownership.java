/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;


import javax.annotation.concurrent.ThreadSafe;
import javax.inject.Inject;
import javax.inject.Provider;

class Obj {
  Object f;
  Obj g;
}

interface CustomProvider<T> extends Provider<T> {

  @Override
  public T get();
}

@ThreadSafe
public class Ownership {

  Obj field;

  public Ownership(Obj o) {
    field = o;
  }

  // understand that ownership can be acquired via DI
  @Inject Ownership(Provider<Obj> objProvider) {
    Obj owned = objProvider.get();
    owned.f = new Object(); // should not report
  }

  @Inject Ownership(CustomProvider<Obj> objProvider) {
    Obj owned = objProvider.get();
    owned.f = new Object(); // should not report
  }

  Obj mInjectedField1;
  Obj mInjectedField2;

  // because this constructor is meant to be called via DI, we assume that injectedField and other
  // parameters passed to the constructor will always be freshly allocated
  @Inject Ownership(Obj injectedField1, Obj injectedField2) {
    mInjectedField1 = injectedField1;
    mInjectedField2 = injectedField2;
    mInjectedField1.f = new Object(); // should not warn
    mInjectedField2.f = new Object(); // should not warn
  }

  Ownership(Obj obj, Object o) {
    obj.f = o; // not annotated @Inject; should warn
  }

  native void leakToAnotherThread(Object o);

  public void escapeViaConstructorBad() {
    Obj local = new Obj();
    Ownership constructed = new Ownership(local);
    local.f = new Object();
  }

  public void ownInOneBranchBad(Obj formal, boolean b) {
    if (b) {
      formal = new Obj();
    }
    // we might not own formal
    formal.f = new Object();
  }

  public void reassignToFormalBad(Obj formal) {
    Obj local = new Obj();
    formal.g = local; // bad, we don't own formal
    formal.g.f = new Object(); // ok; now that formal.g is reassigned to local, which we do own
  }

  public void ownedLocalOk() {
    Obj local = new Obj();
    local.f = new Object();
  }

  public Obj returnOwnedLocalOk() {
    Obj local = new Obj();
    local.f = new Object();
    return local;
  }

  public void writeOwnedLocalThenEscapeOk() {
    Obj local = new Obj();
    local.f = new Object();
    leakToAnotherThread(local);
  }

  public void ownInBranchesOk1(boolean b) {
    Obj local;
    if (b) {
      local = new Obj();
      local.f = new Object();
    } else {
      local = new Obj();
      local.f = new Integer(0);
    }
    local.f = new Boolean(false);
  }

  public void ownedAccessPathOk() {
    Obj local = new Obj();
    local.g = new Obj();
    local.g.f = new Object();
  }

  public void aliasOwnedLocalOk() {
    Obj local = new Obj();
    Obj alias = local;
    alias.f = new Object();
    local.f = new Object();
  }

  public void aliasOwnedLocalAccessPathOk() {
    Obj local = new Obj();
    local.g = new Obj();
    Obj alias = local.g;
    alias.f = new Object();
  }

  private void writeToFormal(Obj formal) {
    formal.f = new Object();
  }

  private void callWriteToFormal(Obj formal) {
    writeToFormal(formal);
  }

  private void setField(Obj o) {
    this.field = o;
  }

  native Obj getMaybeUnownedObj();

  // warn here even though this this is safe if `o` is owned at all call sites. because it's a
  // public method, it's possible to use it in an unsafe way
  public void writeToNotOwnedInCalleeBad1(Obj o) {
    writeToFormal(o);
  }

  public void writeToNotOwnedInCalleeBad2() {
    Obj o = getMaybeUnownedObj();
    writeToFormal(o);
  }

  public void writeToNotOwnedInCalleeBad3(Obj o) {
    callWriteToFormal(o);
  }

  // assuming that we can't own the `this` object
  public void cantOwnThisBad() {
    setField(new Obj());
  }

  public void writeToOwnedInCalleeOk1() {
    Obj o = new Obj();
    writeToFormal(o);
  }

  public void writeToOwnedInCalleeOk2() {
    synchronized (this) {
      this.field = new Obj();
    }
    writeToFormal(this.field);
  }

  public void writeToOwnedInCalleeIndirectOk1() {
    Obj o = new Obj();
    callWriteToFormal(o);
  }

  public void writeToOwnedInCalleeIndirectOk2() {
    Obj o = new Obj();
    o.g = new Obj();
    callWriteToFormal(o.g);
  }

  public Obj ownershipCanBeInterproceduralOk() {
    Obj local = returnOwnedLocalOk();
    local.f = new Object();
    return local;
  }

  public void mutateDoubleReturnOwnedOk() {
    Obj owned = ownershipCanBeInterproceduralOk();
    owned.g = new Obj();
  }

  Obj returnOwnedOrNull(boolean b) {
    if (b) {
      return null;
    }
    return new Obj();
  }

  public void mutateAfterNullCheckOK(boolean b) {
    Obj o = returnOwnedOrNull(b);
    if (o != null) {
      o.f = new Object();
    }
  }

  private void mutateIfNotNull(Obj o) {
    if (o != null) {
      o.f = new Object();
    }
  }

  public void ownInCalleeViaNullOk() {
    mutateIfNotNull(null);
  }

  public void notOwnedInCalleeBad(Obj o) {
    mutateIfNotNull(o);
  }

  Obj id(Obj param) {
    return param;
  }

  public void passOwnershipInIdFunctionOk() {
    Obj owned = new Obj();
    Obj shouldBeOwned = id(owned);
    shouldBeOwned.f = new Object();
  }

  Obj id2(Obj param) {
    return id(param);
  }

  public void passOwnershipInMultiLevelIdFunctionOk() {
    Obj owned = new Obj();
    Obj shouldBeOwned = id2(owned);
    shouldBeOwned.f = new Object();
  }

  native boolean nondet();

  public Obj returnConditionalOwnedInTwoBranches(Obj param) {
    if (nondet()) {
      return param;
    }
    return param;
  }

  public void returnConditionalOwnedInTwoBranchesOk() {
    Obj owned = new Obj();
    Obj shouldBeOwned = returnConditionalOwnedInTwoBranches(owned);
    shouldBeOwned.f = new Object();
  }

  public Obj returnOwnedOrConditionalOwned(Obj param) {
    if (nondet()) {
      return param;
    } else {
      return new Obj();
    }
  }

  public void ownedAfterCastOk() {
    Object o = new Obj();
    Obj owned = (Obj) o;
    owned.f = new Object();
  }

  // TODO: need to handle multiple ownership attributes in order to get this one
  public void FP_ownAndConditionalOwnOk() {
    Obj owned = new Obj();
    Obj shouldBeOwned = returnOwnedOrConditionalOwned(owned);
    shouldBeOwned.f = new Object();
  }

  public Obj twoDifferentConditionalOwns(Obj param1, Obj param2) {
    if (nondet()) {
      return param1;
    } else {
      return param2;
    }
  }

  public void threadLocalOk(ThreadLocal<Obj> threadLocal) {
    threadLocal.get().f = new Object();
  }

  // need to handle multiple ownership attributes in order to get this one
  public void FP_twoDifferentConditionalOwnsOk() {
    Obj owned1 = new Obj();
    Obj owned2 = new Obj();
    Obj shouldBeOwned = twoDifferentConditionalOwns(owned1, owned2);
    shouldBeOwned.f = new Object();
  }

  // we angelically assume that callees don't leak their arguments to another thread for now, so
  // we'll miss this
  public void FN_escapeThenWriteLocalBad() {
    Obj local = new Obj();
    leakToAnotherThread(local);
    local.f = new Object();
  }

  private Obj leakThenReturn() {
    Obj local = new Obj();
    leakToAnotherThread(local);
    return local;
  }

  // the summary for leakThenReturn should not say that the caller owns the return value
  public void FN_mutateReturnedBad() {
    Obj notOwned = leakThenReturn();
    notOwned.f = new Object(); // should warn here
  }

  private void castThenCall(Obj o) {
    Subclass s = (Subclass) o;
    s.doWrite();
  }

  void castThenCallOk() {
    Obj o = new Obj();
    castThenCall(o);
  }

  void castThenCallBad() {
    Obj o = getMaybeUnownedObj();
    castThenCall(o);
  }

  private Obj castThenReturn(Obj o) {
    Subclass s = (Subclass) o;
    return s;
  }

  void castThenReturnOk() {
    Obj o = new Obj();
    castThenReturn(o).f = new Object();
  }

  void castThenReturnBad() {
    Obj o = getMaybeUnownedObj();
    castThenReturn(o).f = new Object();
  }

  void ownViaReflectionOk1() throws InstantiationException, IllegalAccessException {
    Class<Obj> oClass = Obj.class;
    Obj o = oClass.newInstance();
    o.f = new Object();
  }

  void ownViaReflectionOk2()
    throws IllegalAccessException, InstantiationException, InvocationTargetException, NoSuchMethodException {
    Class<Obj> oClass = Obj.class;
    Constructor<Obj> oConstructor = oClass.getConstructor();
    Obj o = oConstructor.newInstance();
    o.f = new Object();
  }

  void ownInSkippedCodeOk() {
    SkippedClass c = SkippedClass.returnOwned();
    c.f = new Object();
  }

  void cloningAquiresOwnershipOk(){
   Ownership ow;
   try {
     ow = (Ownership) this.clone();
     ow.field = null;
   }
   catch (CloneNotSupportedException e) {}
  }

  static MyObj global;

  void storeInGlobalAndWriteBad() {
    MyObj x = new MyObj();
    synchronized(this) { global = x; }
    x.data = 5;
  }

  int readGlobalBad() {
    synchronized(this) { return global.data; }
  }

  public void writeOwnedWithExceptionOk() {
    Obj options = returnOwnedWithException();
    options.f = new Object();
  }

  private Obj returnOwnedWithException() {
    Obj options = new Obj();
    if (options.f==null) {
      throw new IllegalArgumentException();
    }
    return options;
  }

  // not propagating ownership to access path rooted in formal
  public void notPropagatingOwnershipToAccessPathRootedAtFormalBad(Obj m) {
    m.g = new Obj();
  }

  // not propagating ownership to unowned local access path
  public void notPropagatingOwnershipToUnownedLocalAccessPathBad() {
    Obj m;
    synchronized(this) { m = field; }
    m.g = new Obj();
  }

  // propagating ownership to owned access path
  public void propagatingOwnershipToOwnedAccessPathOk() {
    Obj m = new Obj();
    m.g = new Obj();
    m.g.g = new Obj();
    m.g.g.g = new Obj();
  }

}

class MyObj { int data; }

class Subclass extends Obj {

  public void doWrite() {
    f = new Object();
  }

}
