// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

class C {

  public static function gen<T1, T2>(T1 $x, T2 $y, int $opt = 0): T1 {
    return $x;
  }

  public static function call_gen(A $a, B $b): A {
    return self::gen<A, B>($a, $b);
  }

  public static function call_gen_with_opt(A $a, B $b): A {
    return self::gen<A, B>($a, $b, 2);
  }
  public static function genReified<reify T1, reify T2>(
    T1 $x,
    T2 $y,
    int $opt = 0,
  ): T1 {
    return $x;
  }

  public static function call_genReified(A $a, B $b): A {
    return self::genReified<A, B>($a, $b);
  }

  public static function call_genReified_with_opt(A $a, B $b): A {
    return self::genReified<A, B>($a, $b, 1);
  }
}

class A {}

class B {}
