// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace BasicObject;

class A {
  private int $f1;

  public function __construct(int $f1) {
    $this->f1 = $f1;
  }

  public function getField1(): int {
    return $this->f1;
  }

  public function setField1(int $f1): void {
    $this->f1 = $f1;
  }

}

class B extends A {}

class C extends B {
  private int $f2;

  public function __construct(int $f1, int $f2) {
    parent::__construct($f1);
    $this->f2 = $f2;
  }

  public function getField2(): int {
    return $this->f2;
  }

  public function setField2(int $f2): void {
    $this->f2 = $f2;
  }

}

<<__ConsistentConstruct>>
class Base {
  public function __construct(public int $f) {}

  public static function make(int $f): this {
    return new static($f);
  }

  public function copy(): this {
    return new $this($this->f);
  }
}

final class Derived extends Base {
  public function __construct(int $f) {
    parent::__construct($f);
  }

  public function copy(): this {
    return parent::copy();
  }
}

class Main {

  public function set_and_get_A_bad(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $a = new A($u);
    $i0 = $a->getField1();
    $a->setField1($v);
    $i = $a->getField1();
    if ($i0 == $u && $i == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function set_and_get_A_good(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $a = new A($u);
    $i0 = $a->getField1();
    $a->setField1($v);
    $i = $a->getField1();
    if ($i0 != $u || $i != $v) {
      \Level1\taintSink($tainted);
    }
  }

  // testing constuctor inheritance
  public function set_and_get_B_bad(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $a = new B($u);
    $i0 = $a->getField1();
    $a->setField1($v);
    $i = $a->getField1();
    if ($i0 == $u && $i == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function set_and_get_B_good(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $a = new B($u);
    $i0 = $a->getField1();
    $a->setField1($v);
    $i = $a->getField1();
    if ($i0 != $u || $i != $v) {
      \Level1\taintSink($tainted);
    }
  }

  // testing parent::__constuctor call
  public function set_and_get_C_bad(int $u, int $v, int $x, int $y): void {
    $tainted = \Level1\taintSource();
    $a = new C($u, $v);
    $i0 = $a->getField1();
    $j0 = $a->getField2();
    $a->setField1($x);
    $i = $a->getField1();
    $a->setField2($v);
    $j = $a->getField2();
    if ($i0 == $u && $j0 == $v && $i == $x && $j == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function set_and_get_C_good(int $u, int $v, int $x, int $y): void {
    $tainted = \Level1\taintSource();
    $a = new C($u, $v);
    $i0 = $a->getField1();
    $j0 = $a->getField2();
    $a->setField1($x);
    $i = $a->getField1();
    $a->setField2($v);
    $j = $a->getField2();
    if ($i0 != $u || $j0 != $v || $i != $x || $j != $v) {
      \Level1\taintSink($tainted);
    }
  }

  // testing static() constructor
  // see https://docs.hhvm.com/hack/attributes/predefined-attributes#__consistentconstruct
  public function set_and_get_Derived_bad(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $o = Derived::make($u);
    $i0 = $o->f;
    $o->f = $v;
    $i = $o->f;
    if ($i0 == $u && $i == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function set_and_get_Derived_good(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $o = Derived::make($u);
    $i0 = $o->f;
    $o->f = $v;
    $i = $o->f;
    if ($i0 != $u || $i != $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function set_and_get_Derived_copy_bad(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();
    $o1 = Derived::make($u);
    $o2 = $o1->copy();
    $i0 = $o1->f;
    $o1->f = $v;
    $i = $o2->f;
    $o2->f = $w;
    if ($i0 == $u && $o1->f == $v && $i == $u && $o2->f == $w) {
      \Level1\taintSink($tainted);
    }
  }

  public function set_and_get_Derived_copy_good(int $u, int $v, int $w): void {
    $tainted = \Level1\taintSource();
    $o1 = Derived::make($u);
    $o2 = $o1->copy();
    $i0 = $o1->f;
    $o1->f = $v;
    $i = $o2->f;
    $o2->f = $w;
    if ($i0 != $u || $o1->f != $v || $i != $u || $o2->f != $w) {
      \Level1\taintSink($tainted);
    }
  }
}
