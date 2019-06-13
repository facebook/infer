/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.infer.annotation;

import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;

public class Assertions {

  public static <T> T assumeNotNull(@Nullable T object) {
    return object;
  }

  public static <T> T assumeNotNull(@Nullable T object, String explanation) {
    return object;
  }

  public static <T> T assertNotNull(@Nullable T object) {
    if (object == null) {
      throw new AssertionError();
    }
    return object;
  }

  public static <T> T assertNotNull(@Nullable T object, String explanation) {
    if (object == null) {
      throw new AssertionError(explanation);
    }
    return object;
  }

  public static <T> T assertGet(int index, List<T> list) {
    assertCondition(0 <= index && index < list.size(), "Index not in bound");
    return assertNotNull(list.get(index), "Null value");
  }

  public static <K, V> V assertGet(K key, Map<K, V> map) {
    assertCondition(map.containsKey(key), "Key not found");
    return assertNotNull(map.get(key), "Null value");
  }

  public static void assumeCondition(boolean condition) {}

  public static void assumeCondition(boolean condition, String explanation) {}

  public static void assertCondition(boolean condition) {
    if (!condition) {
      throw new AssertionError();
    }
  }

  public static void assertCondition(boolean condition, String explanation) {
    if (!condition) {
      throw new AssertionError(explanation);
    }
  }

  public static AssertionError assertUnreachable() {
    throw new AssertionError();
  }

  public static AssertionError assertUnreachable(String explanation) {
    throw new AssertionError(explanation);
  }

  public static AssertionError assertUnreachable(Exception exception) {
    throw new AssertionError(exception);
  }
}
