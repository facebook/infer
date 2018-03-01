/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
class my_class {
  int idx;
  int arr[10];

  void set_a(int n) { idx = n; }

  int id(int n) { return n; }

 public:
  int access_Bad() {
    set_a(10);
    return arr[idx];
  }

  int access2_Bad() {
    int n = 10;
    return arr[id(n)];
  }

  int access_nth(int n) { return arr[n]; }
};

void access_after_new_Good() {
  my_class* x = new my_class();
  x->access_nth(5);
}

void access_after_new_Bad() {
  my_class* x = new my_class();
  x->access_nth(15);
}

#include <stdlib.h>

class my_class2 {
 public:
  int a[5];
};

void array_member_malloc_Good() {
  my_class2* x = (my_class2*)malloc(sizeof(my_class2));
  x->a[0] = 0;
}

void array_member_malloc_Bad() {
  my_class2* x = (my_class2*)malloc(sizeof(my_class2));
  x->a[10] = 0;
}

class my_class3 {
 public:
  my_class2 b;
};

void array_member_malloc2_Bad() {
  my_class3* x = (my_class3*)malloc(sizeof(my_class3));
  x->b.a[10] = 0;
}

#include <new>

void placement_new_Good() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (mem) my_class2();
  x->a[0] = 0;
}

void placement_new_Bad() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (mem) my_class2();
  x->a[10] = 0;
}

class my_class4 {
 public:
  int a[3];
  int c[3];
  int b[1];
};

void flexible_array1_Good() {
  char* mem = (char*)malloc(sizeof(my_class4) + 4 * sizeof(int));
  my_class4* x = new (mem) my_class4();
  x->b[4] = 0;
}

void flexible_array1_Bad() {
  char* mem = (char*)malloc(sizeof(my_class4) + 4 * sizeof(int));
  my_class4* x = new (mem) my_class4();
  x->b[5] = 0;
}

void flexible_array2_Bad_FN() {
  char* mem = (char*)malloc(4 * sizeof(int) + sizeof(my_class4));
  my_class4* x = new (mem) my_class4();
  x->b[5] = 0;
}

void flexible_array3_Bad_FN() {
  char* mem = (char*)malloc(sizeof(my_class4) + sizeof(int) * 4);
  my_class4* x = new (mem) my_class4();
  x->b[5] = 0;
}

class my_class5 {
 public:
  int d[3];
  int f[3];
  my_class4 e;
};

void flexible_array4_Good() {
  char* mem = (char*)malloc(sizeof(my_class5) + 4 * sizeof(int));
  my_class5* x = new (mem) my_class5();
  x->e.b[4] = 0;
}

void flexible_array4_Bad() {
  char* mem = (char*)malloc(sizeof(my_class5) + 4 * sizeof(int));
  my_class5* x = new (mem) my_class5();
  x->e.b[5] = 0;
}

class Tree {
 private:
  unsigned int children_num;

  Tree(unsigned int children_num) : children_num(children_num) {}

 public:
  void set_child(Tree* child, unsigned int nth) { children[nth] = child; }

  static Tree* NewNode(unsigned int children_num) {
    char* mem =
        (char*)malloc(sizeof(Tree) + (children_num - 1) * sizeof(Tree*));
    return new (mem) Tree(children_num);
  }

  static Tree* NewLeaf() { return new Tree(0); }

 private:
  Tree* children[1];
};

void flexible_array5_Good() {
  Tree* t = Tree::NewNode(3);
  t->set_child(Tree::NewLeaf(), 0);
  t->set_child(Tree::NewLeaf(), 1);
  t->set_child(Tree::NewLeaf(), 2);
}

void flexible_array5_Bad() {
  Tree* t = Tree::NewNode(3);
  t->set_child(Tree::NewLeaf(), 5);
}

void flexible_array_param_access(my_class4* x) { x->b[3] = 0; }

void flexible_array_param_Good() {
  my_class4* x = (my_class4*)malloc(sizeof(my_class4) + 4 * sizeof(int));
  flexible_array_param_access(x);
}

void flexible_array_param_Bad() {
  my_class4* x = (my_class4*)malloc(sizeof(my_class4) + 2 * sizeof(int));
  flexible_array_param_access(x);
}

char* my_malloc() { return (char*)malloc(sizeof(my_class4) + 4 * sizeof(int)); }

void return_class_Good() {
  my_class4* x = (my_class4*)my_malloc();
  x->b[3] = 0;
}

void return_class_Bad() {
  my_class4* x = (my_class4*)my_malloc();
  x->b[5] = 0;
}
