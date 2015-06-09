/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package codetoanalyze.java.infer;

import java.util.HashMap;

public class HashMapModelTest {

  public static int putIntegerTwiceThenGetTwice() {
    HashMap<Integer, Integer> hashMap = new HashMap<>();
    Integer i32 = new Integer(32);
    Integer i52 = new Integer(52);

    hashMap.put(i32, i32);
    hashMap.put(i52, i52);

    Integer a = hashMap.get(i32);
    Integer b = hashMap.get(i52);

    return a.intValue() * b.intValue();
  }

  public static int containsIntegerTwiceThenGetTwice(
    HashMap<Integer, Integer> hashMap
  ) {
    Integer i32 = new Integer(32);
    Integer i52 = new Integer(52);

    if (hashMap.containsKey(i32) && hashMap.containsKey(i52)) {
      Integer a = hashMap.get(i32);
      Integer b = hashMap.get(i52);
      return a.intValue() * b.intValue();
    }

    return 0;
  }

  public static int getOneIntegerWithoutCheck(
    HashMap<Integer, Integer> hashMap
  ) {
    Integer i32 = new Integer(32);

    Integer a = hashMap.get(i32);

    return a.intValue();
  }

  public static int getTwoIntegersWithOneCheck(
    HashMap<Integer, Integer> hashMap
  ) {
    Integer i32 = new Integer(32);
    Integer i52 = new Integer(52);

    if (hashMap.containsKey(i32)) {
      Integer a = hashMap.get(i32);
      Integer b = hashMap.get(i52);

      return a.intValue() * b.intValue();
    }

    return 0;
  }

  public static int getTwoParameterIntegersWithOneCheck(
    HashMap<Integer, Integer> hashMap,
    Integer i32,
    Integer i52
  ) {
    if (hashMap.containsKey(i32)) {
      Integer a = hashMap.get(i32);
      Integer b = hashMap.get(i52);

      return a.intValue() * b.intValue();
    }

    return 0;
  }

}
