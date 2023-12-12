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

  /** Assertion with a runtime check. */
  public static <T> T assertNotNull(@Nullable T object, String explanation) {
    if (object == null) {
      throw new AssertionError(explanation);
    }
    return object;
  }

  /**
   * See {@code #assertNotNull(T object, String explanation)}.
   *
   * <p>NOTE: prefer always providing an explanation.
   */
  public static <T> T assertNotNull(@Nullable T object) {
    if (object == null) {
      throw new AssertionError();
    }
    return object;
  }

  /**
   * Same as {@code #assertNotNull()}, but does not do a runtime check. Useful in
   * performance-critical places when a runtime check is costly.
   *
   * <p>NOTE: Whenever possible, prefer using {@code #assertNotNull()}. Java runtime is doing a good
   * job with optimizing nullability checks, so most likely this won't improve performance, but will
   * make unexpected null propagate and hide the real cause of an error.
   */
  public static <T> T assumeNotNull(@Nullable T object, String explanation) {
    return object;
  }

  /**
   * See {@code #assumeNotNull(T object, String explanation)}.
   *
   * <p>NOTE: prefer always providing an explanation.
   */
  public static <T> T assumeNotNull(@Nullable T object) {
    return object;
  }

  /**
   * Makes Nullsafe (a nullability type checker) stop complaining when {@code object} is
   * dereferenced or converted to a non-nullable. In contrast with {@code #assumeNotNull()},
   * indicates cases when the proper fix needs to be committed, but for some reason it is hard or
   * impossible to do it staight away.
   */
  public static <T> T nullsafeFIXME(@Nullable T object, String explanationOrTask) {
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
