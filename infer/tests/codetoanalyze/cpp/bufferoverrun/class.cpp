/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

void new_nothrow_Good() {
  my_class2* x = new (std::nothrow) my_class2();
  x->a[0] = 0;
}

void new_nothrow_Bad() {
  my_class2* x = new (std::nothrow) my_class2();
  x->a[10] = 0;
}

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

enum class DummyClass {};
inline void* operator new(std::size_t, DummyClass, void* p) { return p; }
inline void* operator new(std::size_t, void* p, DummyClass) { return p; }

void placement_new_overload1_Good() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (DummyClass{}, mem) my_class2();
  x->a[0] = 0;
}

void placement_new_overload1_Bad() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (DummyClass{}, mem) my_class2();
  x->a[10] = 0;
}

void placement_new_overload2_Good() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (mem, DummyClass{}) my_class2();
  x->a[0] = 0;
}

void placement_new_overload2_Bad() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (mem, DummyClass{}) my_class2();
  x->a[10] = 0;
}

struct DummyStruct {};
inline void* operator new(std::size_t, DummyStruct, void* p) { return p; }
inline void* operator new(std::size_t, void* p, DummyStruct) { return p; }

void placement_new_overload3_Good() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (DummyStruct{}, mem) my_class2();
  x->a[0] = 0;
}

void placement_new_overload4_Good() {
  char* mem = (char*)malloc(sizeof(my_class2));
  my_class2* x = new (mem, DummyStruct{}) my_class2();
  x->a[0] = 0;
}

struct Allocator {
  void* allocate(std::size_t size) { return malloc(size); }
};

void* operator new(std::size_t size, Allocator& allocator) {
  return allocator.allocate(size);
}

void user_defined_new_Bad_FN() {
  Allocator allocator;
  my_class2* x = new (allocator) my_class2();
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

void* operator new(std::size_t s1, std::size_t s2) { return malloc(s1 + s2); }
void* operator new(std::size_t s1, std::size_t s2, bool) {
  return malloc(s1 + s2);
}

void flexible_array_new_overload1_Good() {
  my_class4* x = new (5 * sizeof(int)) my_class4();
  x->b[5] = 0;
}

void flexible_array_new_overload1_Bad() {
  my_class4* x = new (5 * sizeof(int)) my_class4();
  x->b[10] = 0;
}

void flexible_array_new_overload2_Good() {
  my_class4* x = new (5 * sizeof(int), true) my_class4();
  x->b[5] = 0;
}

void flexible_array_new_overload2_Bad() {
  my_class4* x = new (5 * sizeof(int), true) my_class4();
  x->b[10] = 0;
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

struct S {
  static constexpr unsigned int x = 32;
};

void use_global_Good() {
  int a[50];
  a[S::x] = 0;
}

void use_global_Bad() {
  int a[30];
  a[S::x] = 0;
}

const unsigned int S::x;

void use_global_2_Good() {
  int a[50];
  a[S::x] = 0;
}

void use_global_2_Bad() {
  int a[30];
  a[S::x] = 0;
}

class my_class6 {
  int* x;

  void dummy_function() {}

  void set_x_two_Good() {
    int arr[5];
    *x = 0;
    dummy_function();
    arr[*x] = 0;
  }

  void set_x_two_Bad() {
    int arr[5];
    *x = 5;
    dummy_function();
    arr[*x] = 0;
  }

  void set_x_three() { *x = 3; }

  void call_set_x_three_Good() {
    int arr[5];
    set_x_three();
    arr[*x] = 0;
  }

  void call_set_x_three_Bad() {
    int arr[3];
    set_x_three();
    arr[*x] = 0;
  }
};
