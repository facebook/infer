/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;

@ThreadSafe
class Containers {

  Map<String,String> mMap;

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

}
