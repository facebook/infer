//
// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
//

class List {

    static class Node {
	int elt;
	Node next;
	Node(int elt, Node next) {
	    this.elt = elt;
	    this.next = next;
	}
    }

    private Node head;

    void push(int elt) {
	this.head = new Node(elt, this.head);
    }

}
