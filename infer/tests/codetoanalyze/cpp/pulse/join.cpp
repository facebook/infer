/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <vector>

void basic_loop_count_ok(int n) {
  for (int i = 0; i < n; i++) {
  }
}

struct foo {
  int* val;
};

struct list {
  struct list* next;
  struct foo* foo;
};

int invalidate_node_alias_latent(struct list* head, int cond) {
  int* result = 0;
  struct list* x = head;
  if (cond) {
    result = x->next->foo->val;
    delete result;
  } else {
    x = x->next;
    struct list* y = x->next;
    result = x->foo->val;
    delete result;
  }
  return *result;
}

void invalidate_node_alias_bad(struct list* head) {
  invalidate_node_alias_latent(head, true);
}

void list_delete_ok(struct list** l) {
  auto head = *l;
  *l = nullptr;
  while (head) {
    auto tmp = head;
    head = head->next;
    if (tmp->foo) {
      free(tmp->foo);
      tmp->foo = nullptr;
    }
  }
}

struct BasicStruct {
  void some_method() {}
  BasicStruct();
  ~BasicStruct();
};

int nested_loops_ok() {
  while (true) {
    BasicStruct x;
    for (;;) {
      x.some_method();
    }
  }
}

extern bool some_bool();
extern BasicStruct mk_basic_struct();

void cond_inside_loop_ok() {
  while (true) {
    BasicStruct x;
    if (some_bool()) {
      x = mk_basic_struct();
    }

    x.some_method();
  }
}

void nested_loops3_ok(std::vector<BasicStruct>* c) {
  for (auto& b : *c) {
    (&b)->~BasicStruct();
  }
}

void duff_switch_loop(char* to, char* from, unsigned int count) {
  unsigned int rounds = (count + 127) / 128;
  switch (count % 128) {
    case 0:
      do {
        *to = *from++;
        case 127:
          *to = *from++;
        case 126:
          *to = *from++;
        case 125:
          *to = *from++;
        case 124:
          *to = *from++;
        case 123:
          *to = *from++;
        case 122:
          *to = *from++;
        case 121:
          *to = *from++;
        case 120:
          *to = *from++;
        case 119:
          *to = *from++;
        case 118:
          *to = *from++;
        case 117:
          *to = *from++;
        case 116:
          *to = *from++;
        case 115:
          *to = *from++;
        case 114:
          *to = *from++;
        case 113:
          *to = *from++;
        case 112:
          *to = *from++;
        case 111:
          *to = *from++;
        case 110:
          *to = *from++;
        case 109:
          *to = *from++;
        case 108:
          *to = *from++;
        case 107:
          *to = *from++;
        case 106:
          *to = *from++;
        case 105:
          *to = *from++;
        case 104:
          *to = *from++;
        case 103:
          *to = *from++;
        case 102:
          *to = *from++;
        case 101:
          *to = *from++;
        case 100:
          *to = *from++;
        case 99:
          *to = *from++;
        case 98:
          *to = *from++;
        case 97:
          *to = *from++;
        case 96:
          *to = *from++;
        case 95:
          *to = *from++;
        case 94:
          *to = *from++;
        case 93:
          *to = *from++;
        case 92:
          *to = *from++;
        case 91:
          *to = *from++;
        case 90:
          *to = *from++;
        case 89:
          *to = *from++;
        case 88:
          *to = *from++;
        case 87:
          *to = *from++;
        case 86:
          *to = *from++;
        case 85:
          *to = *from++;
        case 84:
          *to = *from++;
        case 83:
          *to = *from++;
        case 82:
          *to = *from++;
        case 81:
          *to = *from++;
        case 80:
          *to = *from++;
        case 79:
          *to = *from++;
        case 78:
          *to = *from++;
        case 77:
          *to = *from++;
        case 76:
          *to = *from++;
        case 75:
          *to = *from++;
        case 74:
          *to = *from++;
        case 73:
          *to = *from++;
        case 72:
          *to = *from++;
        case 71:
          *to = *from++;
        case 70:
          *to = *from++;
        case 69:
          *to = *from++;
        case 68:
          *to = *from++;
        case 67:
          *to = *from++;
        case 66:
          *to = *from++;
        case 65:
          *to = *from++;
        case 64:
          *to = *from++;
        case 63:
          *to = *from++;
        case 62:
          *to = *from++;
        case 61:
          *to = *from++;
        case 60:
          *to = *from++;
        case 59:
          *to = *from++;
        case 58:
          *to = *from++;
        case 57:
          *to = *from++;
        case 56:
          *to = *from++;
        case 55:
          *to = *from++;
        case 54:
          *to = *from++;
        case 53:
          *to = *from++;
        case 52:
          *to = *from++;
        case 51:
          *to = *from++;
        case 50:
          *to = *from++;
        case 49:
          *to = *from++;
        case 48:
          *to = *from++;
        case 47:
          *to = *from++;
        case 46:
          *to = *from++;
        case 45:
          *to = *from++;
        case 44:
          *to = *from++;
        case 43:
          *to = *from++;
        case 42:
          *to = *from++;
        case 41:
          *to = *from++;
        case 40:
          *to = *from++;
        case 39:
          *to = *from++;
        case 38:
          *to = *from++;
        case 37:
          *to = *from++;
        case 36:
          *to = *from++;
        case 35:
          *to = *from++;
        case 34:
          *to = *from++;
        case 33:
          *to = *from++;
        case 32:
          *to = *from++;
        case 31:
          *to = *from++;
        case 30:
          *to = *from++;
        case 29:
          *to = *from++;
        case 28:
          *to = *from++;
        case 27:
          *to = *from++;
        case 26:
          *to = *from++;
        case 25:
          *to = *from++;
        case 24:
          *to = *from++;
        case 23:
          *to = *from++;
        case 22:
          *to = *from++;
        case 21:
          *to = *from++;
        case 20:
          *to = *from++;
        case 19:
          *to = *from++;
        case 18:
          *to = *from++;
        case 17:
          *to = *from++;
        case 16:
          *to = *from++;
        case 15:
          *to = *from++;
        case 14:
          *to = *from++;
        case 13:
          *to = *from++;
        case 12:
          *to = *from++;
        case 11:
          *to = *from++;
        case 10:
          *to = *from++;
        case 9:
          *to = *from++;
        case 8:
          *to = *from++;
        case 7:
          *to = *from++;
        case 6:
          *to = *from++;
        case 5:
          *to = *from++;
        case 4:
          *to = *from++;
        case 3:
          *to = *from++;
        case 2:
          *to = *from++;
        case 1:
          *to = *from++;
      } while (--rounds > 0);
  }
}
