/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stddef.h>
#include <stdexcept>

void conditional_buffer_access(int* ptr, unsigned int size) {
  int i;
  if (size < 1) {
  } else if (size < 2) {
    i = *(ptr++);
  } else if (size < 3) {
    i = *(ptr++);
    i = *(ptr++);
  } else if (size < 4) {
    i = *(ptr++);
    i = *(ptr++);
    i = *(ptr++);
  } else if (size < 5) {
    i = *(ptr++);
    i = *(ptr++);
    i = *(ptr++);
    i = *(ptr++);
  }
}

void call_conditional_buffer_access_Good() {
  int a[1];
  conditional_buffer_access(a, 1);
}

void call_conditional_buffer_access_Bad() {
  int a[1];
  conditional_buffer_access(a, 3);
}

void conditional_buffer_access2(unsigned int n) {
  int a[n];
  conditional_buffer_access(a, n);
}

void call_conditional_buffer_access2_1_Good() { conditional_buffer_access2(1); }

void call_conditional_buffer_access2_2_Good() { conditional_buffer_access2(3); }

void conditional_minus(int* ptr, unsigned int size) {
  int i = 0;
  if (ptr != NULL && (i < size - 1)) {
  }
}

void call_conditional_minus_1_Good() { conditional_minus(NULL, 0); }

void call_conditional_minus_2_Good() {
  int a[3];
  conditional_minus(a, 3);
}

void call_conditional_minus_2_Bad() {
  int a[3];
  conditional_minus(a, 0);
}

unsigned int conditional_minus2(int* ptr, unsigned int size) {
  if (ptr != NULL) {
    return (size - 1);
  }
}

void call_conditional_minus2_1_Good() { conditional_minus2(NULL, 0); }

void call_conditional_minus2_2_Good() {
  int a[3];
  conditional_minus2(a, 3);
}

void call_conditional_minus2_2_Bad() {
  int a[3];
  conditional_minus2(a, 0);
}

enum E {
  E_SIZEONE = 0,
  E_SIZETWO = 1,
};

void conditional_buffer_access3(int* ptr, int size) {
  int i;
  switch (ptr[0]) {
    case E_SIZETWO:
      i = ptr[size - 2];
      i = ptr[size - 1];
      break;

    case E_SIZEONE:
      i = ptr[size - 1];
      break;
  }
}

void call_conditional_buffer_access3_1_Good() {
  int a[2];
  a[0] = E_SIZETWO;
  conditional_buffer_access3(a, 2);
}

void call_conditional_buffer_access3_2_Good() {
  int a[1];
  a[0] = E_SIZEONE;
  conditional_buffer_access3(a, 1);
}

void call_conditional_buffer_access3_Bad() {
  int a[1];
  a[0] = E_SIZETWO;
  conditional_buffer_access3(a, 1);
}

void conditional_inequality(int idx) {
  int a[5];
  if (idx == 5) {
  } else {
    a[idx] = 0;
  }
}

void call_conditional_inequality_Good() { conditional_inequality(5); }

void call_conditional_inequality_Bad() { conditional_inequality(6); }

void conditional_inequality_join1(int idx) {
  int a[5];
  if (idx == 5) {
  } else {
    // pruning exp is "idx != 5"
  L:
    // joined pruning exp is "Unknown"
    a[idx] = 0;
  }

  if (idx == 6) {
    // pruning exp is "idx == 6"
    goto L;
  }
}

void call_conditional_inequality_join1_Good_FP() {
  conditional_inequality_join1(5);
}

void call_conditional_inequality_join1_Bad() {
  conditional_inequality_join1(6);
}

void conditional_inequality_join2(int idx) {
  int a[5];
  if (idx == 5) {
  } else {
    // pruning exp is "idx != 5"
  L:
    // joined pruning exp is "idx != [5, 6]"
    a[idx] = 0;
  }

  if (idx != 6) {
    // pruning exp is "idx != 6"
    goto L;
  }
}

void call_conditional_inequality_join2_1_Bad() {
  conditional_inequality_join2(5);
}

void call_conditional_inequality_join2_2_Bad() {
  conditional_inequality_join2(6);
}

void conditional_inequality_depth2(int i) {
  int a[5];
  if (i != 1) {
    a[i] = 0;
  }
}

void conditional_inequality_depth1(int i) {
  if (i != 5) {
    conditional_inequality_depth2(i);
  }
}

void call_conditional_inequality_depth1_1_Good() {
  conditional_inequality_depth1(5);
}

void call_conditional_inequality_depth1_2_Good() {
  conditional_inequality_depth1(1);
}

void call_conditional_inequality_depth1_3_Bad() {
  conditional_inequality_depth1(6);
}

class MyString {
  char* _data = "";
  size_t _size = 0;

 public:
  size_t size() { return _size; }

  char* data() { return _data; }
};

void set_fourth_idx(char* p) { p[3] = '0'; }

void set_fourth_idx_safe(MyString* input) {
  if (input->size() < 4) {
    return;
  }
  set_fourth_idx(input->data());
}

void call_set_fourth_idx_safe_Good() {
  MyString* s = new MyString();
  set_fourth_idx_safe(s);
}

void throw_exception(int i) {
  int a[10];
  if (i >= 10) {
    throw std::runtime_error("throw exception");
  }
  a[i] = 0;
}

void call_throw_exception_Good() { throw_exception(15); }

void call_throw_exception_Bad() { throw_exception(-5); }
