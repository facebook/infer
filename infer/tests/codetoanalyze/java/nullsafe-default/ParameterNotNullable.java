/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.annotation.SuppressLint;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import javax.annotation.Nullable;

public class ParameterNotNullable {

  boolean field = false;

  ParameterNotNullable() {
    testPrimitive(field);
  }

  void testPrimitive(boolean f) {}

  void test(String s) {
    int n = s.length();
  }

  void testN(@Nullable String s) {
    int n = s != null ? s.length() : 0;
  }

  void callNull() {
    String s = null;
    test(s);
  }

  @SuppressLint("ERADICATE_PARAMETER_NOT_NULLABLE")
  void callNullSuppressed() {
    String s = null;
    test(s);
  }

  void callNullable(@Nullable String s) {
    test(s);
  }

  void callNullOK() {
    String s = null;
    testN(s);
  }

  void callNullableOK(@Nullable String s) {
    testN(s);
  }

  private ParameterNotNullable(@Nullable String s) {}

  class Builder {
    ParameterNotNullable getEradicateParameterNotNullable() {
      return new ParameterNotNullable(null);
    }
  }

  public @Nullable String testSystemGetPropertyArgument() {
    String s = System.getProperty(null);
    return s;
  }

  @Nullable
  String testSystemGetenvBad() {
    return System.getenv(null);
  }

  static @Nullable URL testClassGetResourceArgument(Class cls) {
    return cls.getResource(null);
  }

  void threeParameters(String s1, String s2, String s3) {}

  void testThreeParameters() {
    String s = "";
    threeParameters(null, s, s);
    threeParameters(s, null, s);
    threeParameters(s, s, null);
  }

  class ConstructorCall {
    ConstructorCall(int x, String s) {}

    ConstructorCall() {
      this(3, ""); // OK
    }

    ConstructorCall(int x) {
      this(3, null); // NPE
    }
  }

  void indirectSignatureLookupOk(SomeClass c) {
    c.acceptsNullableParameter(null);
  }

  void doesNotAcceptNullableFirstParameter(Object object, boolean test) {}

  void callWithNullableFirstParameter(boolean t1, boolean t2) {
    doesNotAcceptNullableFirstParameter(null, t1 && t2);
  }

  void callWithConditionalAssignment(Object object, boolean test) {
    doesNotAcceptNullableFirstParameter(test ? object : null, test);
  }

  void testImmutableListOfnotNullArguments() {

    Object notNull = new Object();

    ImmutableList.of(null);
    ImmutableList.of(null, null);
    ImmutableList.of(notNull, notNull);
    ImmutableList.of(notNull, null);
  }

  void testImmutableListCopyOfNotNullArguments() {

    Iterable nullIterable = null;
    Iterator nullIterator = null;
    Collection nullCollection = null;

    ImmutableList.copyOf(nullIterable);
    ImmutableList.copyOf(nullIterator);
    ImmutableList.copyOf(nullCollection);
  }

  void testImmutableListSortedCopyOfNotNullArguments() {
    ImmutableList.sortedCopyOf(null, null);
  }

  void testImmutableSetOfnotNullArguments() {

    Object notNull = new Object();

    ImmutableSet.of(null);
    ImmutableSet.of(null, null);
    ImmutableSet.of(notNull, notNull);
    ImmutableSet.of(notNull, null);
    ImmutableSet.of(notNull, null, notNull, null, notNull);
  }

  void testImmutableSetCopyOfNotNullArguments() {

    Iterable nullIterable = null;
    Iterator nullIterator = null;
    Collection nullCollection = null;

    ImmutableSet.copyOf(nullIterable);
    ImmutableSet.copyOf(nullIterator);
    ImmutableSet.copyOf(nullCollection);
  }

  void testImmutableMapOfnotNullArguments() {

    Object notNull = new Object();

    ImmutableMap.of(null, null);
    ImmutableMap.of(notNull, notNull);
    ImmutableMap.of(notNull, null, notNull, null);
  }

  void testImmutableMapCopyOfNotNullArguments() {

    Iterable nullIterable = null;
    Map nullMap = null;

    ImmutableMap.copyOf(nullIterable);
    ImmutableMap.copyOf(nullMap);
  }

  void testParsingNullStringToNumber() {
    String ns = null;
    long l = Long.parseLong(ns);
    int i = Integer.parseInt(ns);
  }
}

interface SomeInterface {
  void acceptsNullableParameter(@Nullable Object object);
}

abstract class SomeClass implements SomeInterface {}
