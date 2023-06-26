/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

public class InvokeDynamic {

  void invokeDynamicThenNpeBad(List<String> list) {
    Object o = null;
    Collections.sort(
        list,
        (String a, String b) -> {
          return b.compareTo(a);
        });
    o.toString();
  }

  void npeInLambdaBad(List<String> list) {
    Collections.sort(
        list,
        (String a, String b) -> {
          Object o = null;
          o.toString();
          return b.compareTo(a);
        });
  }

  // we still don't get this one (even with Javalib lambda rewriting)
  // because Collections.sort is skipped
  void FN_npeViaCaptureBad(List<String> list) {
    String s = null;
    Collections.sort(
        list,
        (String a, String b) -> {
          return s.compareTo(a);
        });
  }

  Integer npeViaSimpleCapture() {
    String s = null;
    Function<String, Integer> f = (s1) -> s.length();
    return f.apply(null);
  }

  Integer npeViaSimpleParamPassing() {
    Function<String, Integer> f = (s) -> s.length();
    return f.apply(null);
  }

  static class A {
    int val;
  }

  static class Fun1 {
    Function<A, A> f1;

    Fun1(Function<A, A> f1) {
      this.f1 = f1;
    }
  }

  static class Fun2 {
    Fun1 f2;

    Fun2(Fun1 f2) {
      this.f2 = f2;
    }
  }

  static class Box {
    A a;

    public Box(A a) {
      this.a = a;
    }

    public Box map(Function<A, A> f) {
      return new Box(f.apply(this.a));
    }

    public Box mapWithFun(Fun2 fun) {
      return new Box(fun.f2.f1.apply(this.a));
    }
  }

  public static A mix(A a1, A a2) {
    if (a1 == null && a2 == null) return null;
    else return new A();
  }

  // this one uses abduction
  public static A sum(A a1, A a2) {
    A a = new A();
    a.val = a1.val + a2.val;
    return a;
  }

  int testBoxMapMixBad() {
    A a0 = null;
    Box b = new Box(null);
    Function<A, A> f = (a) -> mix(a0, a);
    return b.map(f).a.val;
  }

  int testBoxMapMixGood(A a0) {
    Box b = new Box(new A());
    Function<A, A> f = (a) -> mix(a0, a);
    return b.map(f).a.val;
  }

  int testBoxMapSum1Bad(A a0) {
    Box b = new Box(null);
    Function<A, A> f = (a) -> sum(a0, a);
    return b.map(f).a.val;
  }

  int testBoxMapSum2Bad(A a1) {
    Box b = new Box(a1);
    Function<A, A> f = (a) -> sum(null, a);
    return b.map(f).a.val;
  }

  int testBoxMapSumGood() {
    Box b = new Box(new A());
    A a0 = new A();
    Function<A, A> f = (a) -> sum(a0, a);
    return b.map(f).a.val;
  }

  // closure in a field
  int testBoxMapFunMixBad() {
    A a0 = null;
    Box b = new Box(null);
    Function<A, A> f = (a) -> mix(a0, a);
    return b.mapWithFun(new Fun2(new Fun1(f))).a.val;
  }

  int testBoxMapFunMixGood(A a0) {
    Box b = new Box(new A());
    Function<A, A> f = (a) -> mix(a0, a);
    return b.mapWithFun(new Fun2(new Fun1(f))).a.val;
  }

  // nested closure
  Box testBoxMapAuxiliaryGood(A a0, Box b, BiFunction<A, A, A> f) {
    Function<A, A> g = (a) -> f.apply(a0, a);
    return b.map(g);
  }

  int testBoxMapUseAuxiliaryBad() {
    A a0 = null;
    Box b = new Box(null);
    BiFunction<A, A, A> f = (a1, a2) -> mix(a1, a2);
    return testBoxMapAuxiliaryGood(a0, b, f).a.val;
  }

  int testBoxMapUseAuxiliaryGood(A a0) {
    Box b = new Box(new A());
    BiFunction<A, A, A> f = (a1, a2) -> mix(a1, a2);
    return testBoxMapAuxiliaryGood(a0, b, f).a.val;
  }
}
