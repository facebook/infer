/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.annotation.concurrent.ThreadSafe;

import android.support.v4.util.Pools.SynchronizedPool;

class ContainerWrapper {
  private final List<Object> children = new ArrayList<Object>();

  public Object write(Object v) {
    return _write(v);
  }

  private Object _write(Object node)
  {
    children.add(node);
    return this;
  }

}

@ThreadSafe
class Containers {

  List<String> mList;
  Map<String,String> mMap;

  // lists
  void listAddBad1(String s) {
    mList.add(s);
  }

  void listAddBad2(int index, String s) {
    mList.add(index, s);
  }

  void listAddAllBad(Collection<String> c) {
    mList.addAll(c);
  }

  void listClearBad() {
    mList.clear();
  }

  void listRemoveBad1(int index) {
    mList.remove(index);
  }

  void listRemoveBad2(String s) {
    mList.remove(s);
  }

  void listRemoveAllBad(Collection<String> c) {
    mList.removeAll(c);
  }

  void listSetBad(int index, String s) {
    mList.set(index, s);
  }

  void listSubclassWriteBad(ArrayList<String> list, int index) {
    list.remove(index);
  }

  void listReadOk(int index, String s) {
    mList.contains(s);
    mList.get(index);
    mList.isEmpty();
    mList.size();
  }

  void accessSafeListOk(CopyOnWriteArrayList list, int index) {
    list.remove(index);
  }

  // maps
  void mapPutBad(String key, String value) {
    mMap.put(key, value);
  }

  void mapRemoveBad(String key) {
    mMap.remove(key);
  }

  void mapClearBad() {
    mMap.clear();
  }

  void mapPutAllBad(Map<String,String> otherMap) {
    mMap.putAll(otherMap);
  }

  void mapReadsOk(String s) {
    mMap.containsKey(s);
    mMap.containsValue(s);
    mMap.entrySet();
    mMap.hashCode();
    mMap.isEmpty();
    mMap.keySet();
    mMap.size();
    mMap.values();
  }

  // make sure we still warn on subtypes of Map
  void mapSubclassWriteBad(HashMap<String,String> m, String key) {
    m.remove(key);
  }

  synchronized void synchronizedWriteOk1(String key) {
    mMap.remove(key);
  }

  void synchronizedWriteOk2(String key, String lock) {
    synchronized (lock) {
      mMap.remove(key);
    }
  }

  void accessToSychronizedMapsOk(
    String key,
    ConcurrentMap<String,String> concurrentMap,
    ConcurrentHashMap<String,String> concurrentHashMap) {

    concurrentMap.remove(key);
    concurrentHashMap.remove(key);
  }

  public void containerWrapperOwnedWriteOk(Object o) {
    ContainerWrapper wrapper = new ContainerWrapper();
    wrapper.write(o);
  }

  ContainerWrapper mContainerWrapper;

  public void containerWrapperUnownedWriteBad(Object o) {
    mContainerWrapper.write(o);
  }

  static SynchronizedPool<Obj> sPool;

  void poolAcquireOk() {
    Obj obj = sPool.acquire();
    obj.f = new Object();
  }

  void poolAcquireThenNullCheckOk() {
    Obj obj = sPool.acquire();
    if (obj == null) {
      obj = new Obj();
    }
    obj.f = new Object();
  }

  static boolean sUsePooling;

  private Obj poolWrapper() {
    Obj obj = sUsePooling ? sPool.acquire() : null;
    if (obj == null) {
      obj = new Obj();
    }

    return obj;
  }

  void poolWrapperOk() {
    Obj obj = poolWrapper();
    obj.f = new Object();
  }

  // need to understand semantics of release to get this one
  void FN_poolReleaseThenWriteBad() {
    Obj obj = sPool.acquire();
    sPool.release(obj);
    obj.f = new Object(); // should flag
  }

  void release(Obj o) {
    sPool.release(o);
  }

  // we won't catch this without a fancier ownership domain
  void FN_poolReleaseThenWriteInterprocBad() {
    Obj obj = sPool.acquire();
    release(obj);
    obj.f = new Object(); // should flag
  }

  private static List addOrCreateList(List list) {
    if (list == null) {
      list = new ArrayList<>();
    }
    list.add(new Object());
    return list;
  }

  public void addToNullListOk() {
    List list = null;
    addOrCreateList(list);
  }

}
