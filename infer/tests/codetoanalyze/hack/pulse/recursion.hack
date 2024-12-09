// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Recursion;

class Node {
  private Node $next;
  private int $data;

  public function __construct(int $data, Node $next) {
    $this->data = $data;
    $this->next = $next;
  }

  public function length_ok(): int {
    if ($this->next == null) {
      return 1;
    }
    return 1 + $this->next->length_ok();
  }

  // progress in callee
  public function interproc_progress_ok(Node $list): void {
    $this->interproc2_ok($list);
  }

  private function interproc2_ok(Node $list): void {
    $this->interproc_progress_ok($list->next);
  }

  // progress before callee
  public function other_interproc_progress_ok(Node $list): void {
    $this->other_interproc2_ok($list->next);
  }

  private function other_interproc2_ok(Node $list): void {
    $this->other_interproc_progress_ok($list);
  }

  public function infinite_bad(Node $something, Node $other): int {
    return $this->infinite_bad($other, $something);
  }

  public function infinite_same_args_bad(Node $something, Node $other): int {
    return $this->infinite_same_args_bad($something, $other);
  }
}

class A {
  public function __construct(int $i) {}

  public function FP_optional_arg_in_hierarchy_ok(int $i): void {}
}

class AbstractA extends A {
  // FP, no FP if the optional argument is removed
  public function __construct(int $i = 0) {
    parent::__construct($i);
  }

  public function FP_optional_arg_in_hierarchy_ok(int $i = 0): void {
    parent::FP_optional_arg_in_hierarchy_ok($i);
  }
}

final class B extends AbstractA {
  public function __construct(int $i) {
    parent::__construct();
  }

  public function FP_optional_arg_in_hierarchy_ok(int $i = 0): void {
    parent::FP_optional_arg_in_hierarchy_ok();
  }
}

abstract final class AA {
  public static function something(A $a, int $x): void {
    self::something_else($a, $x);
  }

  public static function something_else(A $a, int $x): void {
    BB::something($x, $a);
  }
}

final class BB {
  public static function something(int $x, A $a): void {
    self::something_else($x, $a);
  }

  public static function something_else(int $x, A $a): void {
    AA::something($a, $x);
  }
}

trait InfiniteRecursionInTrait {
  public function infinite_same_args_bad(Node $something, Node $other): int {
    return $this->infinite_same_args_bad($something, $other);
  }
}
