/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

struct Person {
  int age;
  int height;
  int weight;
};

int simple_null_pointer() {
  struct Person* max = 0;
  return max->age;
}

struct Person* Person_create(int age, int height, int weight) {
  struct Person* who = 0;
  return who;
}

int get_age(struct Person* who) { return who->age; }

int null_pointer_interproc() {
  struct Person* joe = Person_create(32, 64, 140);
  return get_age(joe);
}

void fileNotClosed() {
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    char buffer[256];
    // We can easily batch that by separating with space
    write(fd, buffer, strlen(buffer));
  }
}

void simple_leak() {
  int* p;
  p = (int*)malloc(sizeof(int));
}

void common_realloc_leak() {
  int *p, *q;
  p = (int*)malloc(sizeof(int));
  q = (int*)realloc(p, sizeof(int) * 42);
  // if realloc fails, then p becomes unreachable
  if (q != NULL)
    free(q);
}
