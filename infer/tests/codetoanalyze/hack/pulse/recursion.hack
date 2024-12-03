// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

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
}
