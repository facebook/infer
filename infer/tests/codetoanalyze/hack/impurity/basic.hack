// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Basic;

class Object {

  private static int $GLOBAL = 42;

  private int $a;

  public function __construct(int $a) {
    $this->a = $a;
  }

  public function setImpure(int $x, int $y): void {
    $this->a = $x + $y;
  }

  public function getPure(): int {
    return $this->a;
  }

  public function staticSetImpure_FN(int $e): void {
    self::$GLOBAL = $e;
  }

  public static function staticGetPure(): int {
    return self::$GLOBAL;
  }

  public function localWritePure(int $x, int $y): int {
    $k = $x + $y;
    $k++;
    return $k;
  }

  // FP due to $builtins.hhbc_cast_int being unknown
  public function localWritePureWithCast_FP(int $x, ?int $y): int {
    $y = (int)$y;
    $k = $x + $y;
    $k++;
    return $k;
  }

  public function callPurePure(int $size): void {
    for ($i = 0; $i < $size; $i++) {
      $this->localWritePure($i, $size);
    }
  }

  public function callImpureImpure(int $size): void {
    for ($i = 0; $i < $size; $i++) {
      $this->setImpure($i, $size);
    }
  }

  public function localAllocPure(int $x, int $y): void {
    $vec = vec[$x, $y];
    foreach ($vec as $e) {
      $this->callPurePure($e);
    }
  }

  public function parameterFieldWriteImpure(Object $obj, int $i): void {
    $obj->a = $i;
  }

  public function parameterFieldAccessPure(Object $obj): int {
    return $obj->a;
  }

  public function swapImpure(vec<int> $vec, int $i, int $j): void {
    $tmp = $vec[$i];
    $vec[$i] = $vec[$j];
    $vec[$j] = $vec[$tmp];
  }

  public function aliasImpure_FN(vec<int> $vec, int $i, int $j): void {
    $tmp = $vec;
    $tmp[$i] = $j;
  }
}
