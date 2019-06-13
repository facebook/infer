/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <stdio.h>

int test_switch1() {
  int value = 0;
  // infinite loop
  while (value < 10) {
    switch (value) {
      // code before the first case statement gets skipped but can be used to
      // declare variables
      int x = 1;
      printf("(out)HELLO WORLD!");
      x = value + 1;
      case 0:
        printf("(0)HELLO WORLD!");
        break;
      case 1:
        printf("(1)HELLO WORLD!");
        continue;
      case 2:
      default:
        printf("(2/def)HELLO WORLD!");
        continue;
    }
    printf("(after_switch)HELLO WORLD!");
  }
  return 0;
}

int test_switch2() {
  int value = 0;
  switch (value) {
    int x;
    case 0:
      printf("(0)HELLO WORLD!");
      break;
      int z = 9;
    default:

    case 1: {
      int something = 1;
      something++;
    }
      z = 42;
      break;
    case 2:
    case 3: {
    }
  }
  return 0;
}

int test_switch3() {
  int value = 0;
  switch (value) {
    case 0:
      printf("(0)HELLO WORLD!");
      break;
    case 1: {
      int something = 1;
      something++;
    } break;
      int z = 9;
    case 2:
    case 3: {
    }
  }
  return 0;
}

int test_switch4() {
  int value = 0;
  switch (value) {
    int x;
    case 0:
      printf("(0)HELLO WORLD!");
      break;
      int z = 9;
    default:

    case 1: {
      int something = 1;
      something++;
    }
      z = 42;
      break;
    case 2:
    case 3: {
    }
  }
  return 0;
}

int test_switch5() {
  int value = 0;
  while (value < 10) {
    switch (value) {
      int x;
      printf("(out)HELLO WORLD!");
      x = value + 1;
      continue;
      case 0:
        printf("(0)HELLO WORLD!");
        break;
    }
  }
  return 0;
}

int test_switch6() {
  int value = 0;
  switch (value > 0 ? 1 : 0) {
    case 0:
      printf("(0)HELLO WORLD!");
      break;
    case 1: {
      int something = 1;
      something++;
    } break;
      int z = 9;
    case 2:
    case 3: {
    }
  }
  return 0;
}

int getValue() { return 1; }

int test_switch7() {
  int value = 0;
  switch (getValue()) {
    case 0:
      printf("(0)HELLO WORLD!");
      break;
    case 1: {
      int something = 1;
      something++;
    } break;
      int z = 9;
    case 2:
    case 3: {
    }
  }
  return 0;
}

int test_switch8() {
  int value = 0;
  while (value < 10) {
    switch (getValue() == 0 ? 1 : 2) {
      case 0:
        printf("(0)HELLO WORLD!");
        return 0;
      case 1: {
        int something = 1;
        something++;
        continue;
      } break;
        int z = 9;
      case 2:
      case 3: {
      }
    }
    int a = 0;
  }
  return 0;
}

int test_switch9() {
  int value = 0;
  switch (value) {}
  return 0;
}

int test_switch10() {
  int value = 0;
  switch (value = 7) {}
  return 0;
}

int test_switch11() {
  int value = 0;
  switch (value = (value == 0 ? 7 : 9)) {
    case 0:
      printf("(0)HELLO WORLD!");
  }
  return 0;
}

int switch_gnu_range(char c) {
  int i;
  switch (c) {
    case 'a' ... 'f':
      return 0;
      break;
    case '0' ... /* some long comment to make the end of the range on the next
                    line */
        '9':
      i = atoi(c);
      break;
  }
  return i;
}
