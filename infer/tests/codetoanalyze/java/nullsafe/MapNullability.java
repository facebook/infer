/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import javax.annotation.Nullable;

/** Check how we model the behavior of Map nullability */
public class MapNullability {

  class TestThatGetIsAllowedOnlyAfterContainsKeyWasChecked {

    void usingGetAfterKeyWasCheckedIsOK(java.util.Map<Integer, String> m) {
      if (m.containsKey(3)) {
        m.get(3).isEmpty();
      }
    }

    void usingGetWithoutCheckingKeyIsBAD(java.util.Map<Integer, String> m) {
      m.get(3).isEmpty();
    }

    void usingGetAfterWrongKeyWasCheckedIsBAD(java.util.Map<Integer, String> m) {
      if (m.containsKey(3)) {
        m.get(4).isEmpty();
      }
    }

    void usingGetAfterKeyWasCheckedInWhileLoopIsOK(java.util.Map<Integer, String> m) {
      while (true) {
        if (m.containsKey(3)) {
          m.get(3).isEmpty();
        }
      }
    }

    void usingGetAfterWrongKeyWasCheckedInWhileLoopIsBAD(java.util.Map<Integer, String> m) {
      while (true) {
        if (m.containsKey(3)) {
          m.get(4).isEmpty();
        }
      }
    }

    void immutableMap_usingGetAfterKeyWasCheckedIsOK(
        com.google.common.collect.ImmutableMap<Integer, String> m) {
      if (m.containsKey(3)) {
        m.get(3).isEmpty();
      }
    }

    void immutableMap_usingGetAfterWrongKeyWasCheckedIsBAD(
        com.google.common.collect.ImmutableMap<Integer, String> m) {
      if (m.containsKey(3)) {
        m.get(4).isEmpty();
      }
    }
  }

  public class TestThatGetAfterPutIsAllowed {
    String dontAssignNull = "";

    public void getAfterPutIsOK(java.util.Map<String, String> map, String key) {
      map.put(key, "abc");
      dontAssignNull = map.get(key);
    }

    public void getWithoutPutIsBAD(java.util.Map<String, String> map, String key) {
      dontAssignNull = map.get(key);
    }

    public void getAfterPutWrongKeyIsBAD(
        java.util.Map<String, String> map, String key, String wrongKey) {
      map.put(key, "abc");
      dontAssignNull = map.get(wrongKey);
    }

    public void getAfterPutSeveralKeysIsOK(java.util.Map<String, String> map) {
      map.put("key1", "value1");
      map.put("key2", "value1");
      dontAssignNull = map.get("key2");
      dontAssignNull = map.get("key1");
      dontAssignNull = map.get("key2");
      map.put("key3", "value1");
      dontAssignNull = map.get("key1");
      dontAssignNull = map.get("key2");
      dontAssignNull = map.get("key3");
    }

    public void getAfterPutSeveralKeysButGetWrongOneIsBAD(java.util.Map<String, String> map) {
      map.put("key1", "value1");
      map.put("key2", "value1");
      dontAssignNull = map.get("key2"); // OK
      dontAssignNull = map.get("wrong_key"); // BAD
    }

    public void getAfterPutNonnullIsOK(java.util.Map<String, String> map, String nonnullValue) {
      map.put("key", nonnullValue);
      dontAssignNull = map.get("key");
    }

    public void getAfterPutNullableIsBAD(
        java.util.Map<String, String> map, @Nullable String nullableValue) {
      map.put("key", nullableValue);
      dontAssignNull = map.get("key");
    }

    public void overwriteKeyByNullIsBAD(java.util.Map<String, String> map, String key) {
      map.put(key, "abc");
      map.put(key, null); // Parameter not nullable
      dontAssignNull = map.get(key); // BAD
    }

    public void overwriteKeyByNonnullIsOK(java.util.Map<String, String> map, String key) {
      map.put(key, null); // Parameter not nullable
      map.put(key, "abc");
      dontAssignNull = map.get(key); // OK
    }

    public void getAfterConditionalPutIsOK(java.util.Map<String, String> map, String key) {
      if (!map.containsKey(key)) {
        map.put(key, "abc");
      }
      // OK: map either already contained a key, or we've just put it here!
      dontAssignNull = map.get(key);
    }

    public void getAfterConditionalPutWrongKeyIsBAD(
        java.util.Map<String, String> map, String key, String wrongKey) {
      if (!map.containsKey(key)) {
        map.put(wrongKey, "abc");
      }
      dontAssignNull = map.get(key);
    }
  }
}
