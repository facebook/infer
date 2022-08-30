/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import android.os.Bundle;
import androidx.collection.ArrayMap;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

class Test {

  void inefficient_loop_bad(HashMap<String, Integer> testMap) {
    for (String key : testMap.keySet()) {
      testMap.get(key);
    }
  }

  void inefficient_loop_itr_bad(HashMap<String, Integer> testMap) {

    Iterator itr2 = testMap.keySet().iterator();
    while (itr2.hasNext()) {
      String key = (String) itr2.next();
      testMap.get(key);
    }
  }

  void inefficient_loop_itr_heur_bad_FN(HashMap<String, Integer> testMap) {

    Iterator itr2 = testMap.keySet().iterator();
    int i = 0;
    int j = 1;
    int k = 2;
    int l = 3;
    while (itr2.hasNext()) {
      String key = (String) itr2.next();
      testMap.get(key);
    }
  }

  void inefficient_loop_itr_heur_bad(HashMap<String, Integer> testMap) {

    Iterator itr2 = testMap.keySet().iterator();
    int i = 0;
    while (itr2.hasNext()) {
      String key = (String) itr2.next();
      testMap.get(key);
    }
  }

  void inefficient_loop_itr_heur_btw_bad(HashMap<String, Integer> testMap) {

    Set<String> keySet = testMap.keySet();
    int i = 0;
    int j = 1;
    int l = 3;
    Iterator itr2 = keySet.iterator();
    while (itr2.hasNext()) {
      String key = (String) itr2.next();
      testMap.get(key);
    }
  }

  void efficient_loop_itr_ok(HashMap<String, Integer> testMap) {

    Iterator<Map.Entry<String, Integer>> itr1 = testMap.entrySet().iterator();
    while (itr1.hasNext()) {
      Map.Entry<String, Integer> entry = itr1.next();
      entry.getKey();
      entry.getValue();
    }
  }

  void efficient_loop_ok(HashMap<String, Integer> testMap) {
    for (Map.Entry<String, Integer> entry : testMap.entrySet()) {
      entry.getKey();
      entry.getValue();
    }
  }

  void negative_loop_ok(HashMap<String, Integer> testMap1, HashMap<String, Integer> testMap2) {
    for (String key : testMap1.keySet()) {
      testMap2.get(key);
    }
  }

  // Bundle doesn't implement Map hence have any entrySet
  public void from_bundle_ok(Bundle extras) {
    for (String key : extras.keySet()) {
      Object t = extras.get(key);
    }
  }

  // ArrayMap extends SimpleMap.
  void inefficient_arraymap_loop_bad(ArrayMap<String, Integer> arrayMap) {
    for (String key : arrayMap.keySet()) {
      arrayMap.get(key);
    }
  }

  void independent_itr_loop_ok(
      HashMap<String, Integer> testMap, HashMap<Integer, String> otherMap) {
    Set<Integer> s = otherMap.keySet(); // irrelevant
    for (Map.Entry<String, Integer> entry : testMap.entrySet()) {
      String elem =
          otherMap.get(entry.getKey()); // getter is not coming from the one we are iterating over
    }
  }
}
