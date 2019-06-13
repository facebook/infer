/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.HashMap;
import java.util.Map;

class MapTest {

  public void keySet_linear(Map<String, Integer> map) {
    for (String name : map.keySet()) {}
  }

  public void entrySet_linear(Map<String, Integer> map) {
    for (Map.Entry<String, Integer> entry : map.entrySet()) {}
  }

  public void values_linear(Map<String, Integer> map) {
    map.put("hi", 0);
    for (Integer name : map.values()) {}
  }

  public void putAll_linear(Map<String, Integer> map) {
    Map<String, Integer> newmap = new HashMap<>();
    newmap.putAll(map);
    for (Integer name : newmap.values()) {}
  }
}
