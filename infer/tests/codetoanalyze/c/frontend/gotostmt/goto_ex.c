/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <stdio.h>

int getValue() { return 2; }

int g0() {
  int a = 0;
  if (getValue() > 1)
    goto stepC;

stepB:
stepC:
stepD:
  a = 1;
  return 1;
}

int g1() {
  int a = 0;
  if (getValue() > 1)
    goto stepB;
  return 0;

stepB:
  a = 1;
  return 1;
}

int g2() {
  int a = 0;
stepB:
  a = 1;

  if (!getValue())
    goto exit_step;
  if (!getValue())
    goto stepA;
  if (getValue() > 1)
    goto stepB;
  return 0;

stepA : {
  a = 2;
  return 2;
}
exit_step:
  a = 3;
  return 1;
}

int g3() {
stepB:
  printf("B\n");

  if (!getValue())
    goto exit_step;
  if (!getValue())
    goto stepA;
  if (getValue() > 1)
    goto stepB;
  printf("g3\n");
  return 0;

stepA : {
  int a = 2;
  printf("A\n");
}
exit_step:
  printf("exit\n");
  return 1;
}

int g4() {
stepB:
  printf("B\n");

  if (!getValue())
    goto exit_step;
  if (!getValue())
    goto stepA;
  if (getValue() > 1)
    goto stepB;
  printf("g4\n");

stepA : {
  int a = 2;
  printf("A\n");
}
exit_step:
  printf("exit\n");
  return 1;
}

int g5() {
stepB:
  printf("B\n");

  if (!getValue())
    goto exit_step;
  if (!getValue())
    goto stepA;
  if (getValue() > 1)
    goto stepB;
  goto exit_step;

stepA : {
  int a = 2;
  printf("A\n");
  return 1;
}
exit_step:
  printf("exit\n");
  goto stepA;
}

int g6() {
stepB:
  printf("B\n");

  if (!getValue())
    goto exit_step;
  if (!getValue())
    goto stepA;
  if (getValue() > 1)
    goto stepB;
  goto exit_step;

stepA : {
  int a = 2;
  printf("A\n");
}
  return 1;
exit_step:
  printf("exit\n");
  goto stepA;
}

int g7() {
  int i = 0, j = 0, k = 0;
  while (i < 10) {
    while (j < 10) {
      while (k < 10) {
        int v = i + j + k;
        if (v >= 15) {
          goto out;
        print:
          printf("wow\n");
          goto terminate;
        }
      }
    }
  }
out:
  printf("out!\n");
  goto print;
terminate:
  printf("terminating!\n");
  return 2;
}

int g8(int q) {
  int i = 0, j = 0, k = 0;
  if (q)
    goto print;
  while (i < 10) {
    while (j < 10) {
      while (k < 10) {
        int v = i + j + k;
        if (v >= 15) {
        print:
          printf("wow\n");
          // goto terminate;
        }
      }
    }
  }
out:
  printf("out!\n");
terminate:
  printf("terminating!\n");
  return 2;
}
