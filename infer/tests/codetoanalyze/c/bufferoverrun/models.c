/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdio.h>
#include <stdlib.h>

void exit_bo_good_unreachable_bad() {
  int arr[1];
  exit(1);
  // unreachable so no buffer overrun
  arr[42] = 42;
}

void fgetc_m1_bad(FILE* f) {
  int arr[10000];
  int c = fgetc(f);
  arr[c] = 42;
}

void fgetc_255_bad(FILE* f) {
  int arr[255];
  int c = fgetc(f);
  if (c >= 0) {
    arr[c] = 42;
  }
}

void fgetc_256_good(FILE* f) {
  int arr[256];
  int c = fgetc(f);
  if (c >= 0) {
    arr[c] = 42;
  }
}

void fgetc_256_bad(FILE* f) {
  int arr[256];
  int c = fgetc(f);
  arr[c + 1] = 42;
}

void fgetc_257_good(FILE* f) {
  int arr[257];
  int c = fgetc(f);
  arr[c + 1] = 42;
}

void memcpy_bad1() {
  int arr1[10];
  int arr2[20];
  memcpy(arr1, arr2, 44);
}

void memcpy_bad2() {
  int arr1[10];
  int arr2[20];
  memcpy(arr2, arr1, 44);
}

void memcpy_bad3() {
  int arr1[10];
  int arr2[20];
  memcpy(arr1, arr2, -1);
}

void memcpy_bad4() {
  int src[1];
  int buff[1];
  int* dst = &buff[0];
  memcpy(dst, src, sizeof(dst));
}

void memcpy_good1() {
  int arr1[10];
  int arr2[20];
  memcpy(arr2, arr1, 40);
}

void memcpy_good2() {
  int arr1[10];
  int arr2[20];
  memcpy(arr2, arr1, 0);
}

void memcpy_good3() {
  int arr1[10];
  int arr2[20];
  memcpy(arr2, arr1, 20);
}

void memcpy_good4() {
  int src[3];
  int dst[3];
  memcpy(dst, src, sizeof(dst));
}

void memcpy_len_Good(size_t len) {
  char dst[len];
  char src[len];
  memcpy(dst, src, len);
}

void call_memcpy_len1_Good() { memcpy_len_Good(40); }

extern size_t unknown_uint();

void call_memcpy_len2_Good() {
  size_t x = unknown();
  memcpy_len_Good(x);
}

void memmove_bad1() {
  int arr1[10];
  int arr2[20];
  memmove(arr1, arr2, 44);
}

void memmove_bad2() {
  int arr1[10];
  int arr2[20];
  memmove(arr2, arr1, 44);
}

void memmove_bad3() {
  int arr1[10];
  int arr2[20];
  memmove(arr1, arr2, -1);
}

void memmove_bad4() {
  int src[1];
  int buff[1];
  int* dst = &buff[0];
  memmove(dst, src, sizeof(dst));
}

void memmove_good1() {
  int arr1[10];
  int arr2[20];
  memmove(arr2, arr1, 40);
}

void memmove_good2() {
  int arr1[10];
  int arr2[20];
  memmove(arr2, arr1, 0);
}

void memmove_good3() {
  int arr1[10];
  int arr2[20];
  memmove(arr2, arr1, 20);
}

void memmove_good4() {
  int src[3];
  int dst[3];
  memmove(dst, src, sizeof(dst));
}

void memset_bad1() {
  int arr[10];
  memset(arr, 0, 44);
}

void memset_bad2() {
  int arr[10];
  memset(arr, 0, -1);
}

void memset_bad3() {
  int arr[1];
  int* dst = &arr[0];
  memset(dst, 0, sizeof(dst));
}

void memset_good1() {
  int arr[10];
  memset(arr, 0, 40);
}

void memset_good2() {
  int arr[10];
  memset(arr, 0, 0);
}

void memset_good3() {
  int arr[10];
  memset(arr, 0, 20);
}

void memset_good4() {
  int arr[10];
  memset(arr, 0, sizeof(arr));
}

void strncpy_bad1() {
  int arr1[10];
  int arr2[20];
  strncpy(arr1, arr2, 44);
}

void strncpy_bad2() {
  int arr1[10];
  int arr2[20];
  strncpy(arr2, arr1, 44);
}

void strncpy_bad3() {
  int arr1[10];
  int arr2[20];
  strncpy(arr1, arr2, -1);
}

void strncpy_bad4() {
  int src[1];
  int buff[1];
  int* dst = &buff[0];
  strncpy(dst, src, sizeof(dst));
}

void strncpy_good1() {
  int arr1[10];
  int arr2[20];
  strncpy(arr2, arr1, 40);
}

void strncpy_good2() {
  int arr1[10];
  int arr2[20];
  strncpy(arr2, arr1, 0);
}

void strncpy_good3() {
  int arr1[10];
  int arr2[20];
  strncpy(arr2, arr1, 20);
}

void strncpy_good4() {
  int src[3];
  int dst[3];
  strncpy(dst, src, sizeof(dst));
}

void strncpy_good5_FP() {
  char src[5] = "test";
  char dst[5];
  strncpy(dst, src, 10);
}

void memcpy_contents_Good() {
  int src[3] = {5, 5, 5};
  int dst[3];
  memcpy(dst, src, sizeof(dst));
  int a[6];
  a[dst[0]] = 0;
}

void memcpy_contents_Bad() {
  int src[3] = {5, 5, 5};
  int dst[3];
  memcpy(dst, src, sizeof(dst));
  int a[5];
  a[dst[0]] = 0;
}

void memcpy_integer_Good() {
  int src = 5;
  int dst;
  memcpy(&dst, &src, sizeof(int));
  int a[10];
  a[dst] = 0;
}

void memcpy_integer_Bad() {
  int src = 5;
  int dst;
  memcpy(&dst, &src, sizeof(int));
  int a[5];
  a[dst] = 0;
}

void strncpy_contents_Good() {
  char src[5] = "test";
  char dst[10];
  strncpy(dst, src, 5);
  int a[5];
  a[strlen(dst)] = 0;
}

void strncpy_contents_Bad() {
  char src[5] = "test";
  char dst[10];
  strncpy(dst, src, 5);
  int a[4];
  a[strlen(dst)] = 0;
}

void strncpy_no_null_1_Bad_FN() {
  char src[5] = "test";
  src[4] = 'a';
  char dst[10];
  strncpy(dst, src, 5); // [dst] may not have null character.
  int a[10];
  a[strlen(dst)] = 0;
}

void strncpy_no_null_2_Bad() {
  char src[5] = "test";
  src[4] = 'a';
  char dst[10];
  strncpy(dst, src, 5); // [dst] may not have null character.
  int a[5];
  a[strlen(dst)] = 0;
}

void strncpy_no_null_3_Bad_FN() {
  char src[15] = "test.test.test";
  char dst[10];
  strncpy(dst, src, 10); // [dst] does not have null character.
  int a[20];
  a[strlen(dst)] = 0;
}

void strncpy_no_null_4_Bad() {
  char src[15] = "test.test.test";
  char dst[10];
  strncpy(dst, src, 10); // [dst] does not have null character.
  int a[10];
  a[strlen(dst)] = 0;
}

void strndup_1_Good() {
  char s1[] = "Hello Infer!";
  int len = strlen(s1);
  char* s2 = strndup(s1, len);
  for (int i = 0; i < len; i++) {
    s2[i] = 'a';
  }
}

void strndup_1_Bad() {
  char s1[] = "Hello Infer!";
  int len = 100;
  char* s2 = strndup(s1, len);
  for (int i = 0; i < len; i++) {
    s2[i] = 'a';
  }
}

void strndup_2_Good() {
  char s1[] = "Hello Infer!";
  int len = strlen(s1);
  s1[5] = '\0';
  char* s2 = strndup(s1, len);
  for (int i = 0; i < strlen(s2); i++) {
    s2[i] = 'a';
  }
}

void strndup_2_Bad() {
  char s1[] = "Hello Infer!";
  int len = strlen(s1);
  s1[5] = '\0';
  char* s2 = strndup(s1, len);
  for (int i = 0; i < len; i++) {
    s2[i] = 'a';
  }
}

void strndup_3_Good() {
  int size = unknown();
  if (size >= 10) {
    char* s1 = (char*)malloc(sizeof(char) * size);
    s1[5] = '\0';
    char* s2 = strndup(s1, size - 1);
    for (int i = 0; i < strlen(s2); i++) {
      s2[i] = 'a';
    }
  }
}

void strndup_3_Bad() {
  int size = unknown();
  if (size >= 10) {
    char* s1 = (char*)malloc(sizeof(char) * size);
    s1[5] = '\0';
    char* s2 = strndup(s1, size - 1);
    for (int i = 0; i < size - 1; i++) {
      s2[i] = 'a';
    }
  }
}

void strcpy_Good() {
  char src[5] = "test";
  char dst[10];
  strcpy(dst, src);
}

void strcpy_Bad() {
  char src[5] = "test";
  char dst[4];
  strcpy(dst, src);
}

void strcpy_literal_string_Good() {
  char dst[10];
  strcpy(dst, "test");
}

void strcpy_literal_string_Bad() {
  char dst[4];
  strcpy(dst, "test");
}

void strcpy_strlen_Good() {
  char src[5] = "test";
  char dst[10];
  strcpy(dst, src);
  int a[5];
  a[strlen(dst)] = 0;
}

void strcpy_strlen_Bad() {
  char src[5] = "test";
  char dst[10];
  strcpy(dst, src);
  int a[4];
  a[strlen(dst)] = 0;
}

void strcpy_contents_Good() {
  char src[5] = "aaaa";
  char dst[5];
  strcpy(dst, src);
  char c = dst[0];
  if (c >= 'a') {
    int a[5];
    a[c - 'a' + 4] = 0;
  }
}

void strcpy_contents_Bad() {
  char src[5] = "aaaa";
  char dst[5];
  strcpy(dst, src);
  char c = dst[0];
  if (c >= 'a') {
    int a[5];
    a[c - 'a' + 5] = 0;
  }
}

void strcpy_no_null_Bad() {
  char src[5] = "test";
  src[4] = 'a';
  char dst[10];
  strcpy(dst, src);
}

void strcat_Good() {
  char str[8];
  strcpy(str, "abc");
  strcat(str, "defg");
}

void strcat_Bad() {
  char str[8];
  strcpy(str, "abc");
  strcat(str, "defgh");
}

void strcat_strlen_Good() {
  char str[8];
  strcpy(str, "abc");
  strcat(str, "defg");
  int a[8];
  a[strlen(str)] = 0;
}

void strcat_strlen_Bad() {
  char str[20];
  strcpy(str, "abc");
  strcat(str, "defgh");
  int a[8];
  a[strlen(str)] = 0;
}
