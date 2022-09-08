/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class ListTest {
  public static void main(String[] args) {
    Node c1 = new Node();
    for (int i = 0; i <= 2; i++) {
      c1.val = new Value(i);
      Node c2 = new Node();
      c2.next = c1;
      c1 = c2;
    }
  }
}

class Node {
  public Value val;
  public Node next;

  public Node() {
    this.val = null;
    this.next = null;
  }
}

class Value {
  public int x;

  public Value(int x) {
    this.x = x;
  }
}
