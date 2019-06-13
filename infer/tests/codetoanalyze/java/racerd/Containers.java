/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import android.support.v4.util.Pools;
import android.support.v4.util.Pools.SimplePool;
import android.support.v4.util.Pools.SynchronizedPool;
import android.support.v4.util.SimpleArrayMap;
import android.support.v4.util.SparseArrayCompat;
import android.util.SparseArray;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.annotation.concurrent.ThreadSafe;

class ContainerWrapper {
  private final List<Object> children = new ArrayList<Object>();

  public Object write(Object v) {
    return _write(v);
  }

  private Object _write(Object node) {
    children.add(node);
    return this;
  }
}

@ThreadSafe
class Containers {

  List<String> mList;
  Map<String, String> mMap;

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

  List mListNobodyWrites;

  void listReadOk(int index, String s) {
    mListNobodyWrites.contains(s);
    mListNobodyWrites.get(index);
    mListNobodyWrites.isEmpty();
    mListNobodyWrites.size();
  }

  List mListSyncWrites;

  synchronized void listSyncAddBad(String s) {
    mListSyncWrites.add(s);
  }

  boolean listReadBad(String s) {
    return mListSyncWrites.contains(s);
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

  void mapPutAllBad(Map<String, String> otherMap) {
    mMap.putAll(otherMap);
  }

  Map<String, String> mMapNobodyWrites;

  void mapReadsOk(String s) {
    mMapNobodyWrites.containsKey(s);
    mMapNobodyWrites.containsValue(s);
    mMapNobodyWrites.entrySet();
    mMapNobodyWrites.hashCode();
    mMapNobodyWrites.isEmpty();
    mMapNobodyWrites.keySet();
    mMapNobodyWrites.size();
    mMapNobodyWrites.values();
  }

  // make sure we still warn on subtypes of Map
  void mapSubclassWriteBad(HashMap<String, String> m, String key) {
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
      ConcurrentMap<String, String> concurrentMap,
      ConcurrentHashMap<String, String> concurrentHashMap) {

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

  private Obj poolWrapper1() {
    Obj obj = sUsePooling ? sPool.acquire() : null;
    if (obj == null) {
      obj = new Obj();
    }

    return obj;
  }

  void poolWrapperOk1() {
    Obj obj = poolWrapper1();
    obj.f = new Object();
  }

  private Pools.Pool<Obj> mPool;
  private boolean mIsSync;

  private Obj poolWrapper2() {
    Obj item;
    if (mIsSync) {
      synchronized (this) {
        item = mPool.acquire();
      }
    } else {
      item = mPool.acquire();
    }
    return item;
  }

  void poolWrapperOk2() {
    Obj obj = poolWrapper2();
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
    // we get list |->  ({ OwnedIf (0) } |_| { Owned }) here, which simplifies to list |-> {} due
    // to limitations in AttributeMapDomain (join is just simple intersection)
    list.add(new Object());
    return list;
  }

  public void addToNullListOk() {
    List list = null;
    addOrCreateList(list);
  }

  void addToSparseArrayCompatOk() {
    SparseArrayCompat sparseArray = new SparseArrayCompat();
    sparseArray.put(0, new Object());
  }

  public void addToSparseArrayCompatBad(SparseArrayCompat sparseArray) {
    sparseArray.put(0, new Object());
  }

  public void addToSparseArrayOk() {
    SparseArray sparseArray = new SparseArray();
    sparseArray.put(0, new Object());
  }

  public void addToSparseArrayBad(SparseArray sparseArray) {
    sparseArray.put(0, new Object());
  }

  SimpleArrayMap<Integer, Integer> si_map = new SimpleArrayMap<Integer, Integer>();

  public synchronized void addToSimpleArrayMapOk() {
    si_map.put(1, 1);
  }

  public void addToSimpleArrayMapBad(SimpleArrayMap<Integer, Integer> map) {
    map.put(1, 1);
  }

  // this should be a read/write race with addToSimpleArrayMapOk
  public int readSimpleArrayMap() {
    return si_map.get(1);
  }

  SimplePool<Integer> simplePool = new SimplePool<Integer>(10);

  public synchronized Integer getFromPoolOK() {
    return simplePool.acquire();
  }

  public void poolBad() {
    Integer a;
    synchronized (this) {
      a = simplePool.acquire();
    }
    simplePool.release(a);
  }

  Map<String, String> mAliasedMap;

  // won't report here because the read happens through an alias
  public String FN_AliasedMapBad() {
    synchronized (this) {
      mAliasedMap.put("a", "b");
    }
    Map<String, String> alias = mAliasedMap;
    return alias.get("a");
  }
}
