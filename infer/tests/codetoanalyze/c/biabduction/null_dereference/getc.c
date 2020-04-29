/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>
#include <stdlib.h>

void crash_getc() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  i = getc(f);
  printf("i =%i\n", i);
  fclose(f);
}

void nocrash_getc() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    i = getc(f);
    printf("i =%i\n", i);
    fclose(f);
  }
}

void crash_fgetc() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  i = fgetc(f);
  printf("i =%i\n", i);
  fclose(f);
}

void nocrash_fgetc() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    i = fgetc(f);
    printf("i =%i\n", i);
    fclose(f);
  }
}

void crash_ungetc() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");

  int i = ungetc(10, f);
  fclose(f);
}

void nocrash_ungetc() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    int i = ungetc(10, f);
    fclose(f);
  }
}

void crash_fputs() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fputs("blablabla", f);
  fclose(f);
}

void nocrash_fputs() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fputs("blablabla", f);
    fclose(f);
  }
}

void crash_fputc() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fputc(42, f);
  fclose(f);
}

void nocrash_fputc() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fputc(42, f);
    fclose(f);
  }
}

void crash_putc() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  putc(42, f);
  fclose(f);
}

void nocrash_putc() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    putc(42, f);
    fclose(f);
  }
}

void crash_fseeks() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fseek(f, 7, SEEK_SET);
  fclose(f);
}

void nocrash_fseek() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fseek(f, 7, SEEK_SET);
    fclose(f);
  }
}

void crash_ftell() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  ftell(f);
  fclose(f);
}

void nocrash_ftell() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    ftell(f);
    fclose(f);
  }
}

void crash_fgets() {
  FILE* f;
  char str[60];

  f = fopen("this_file_doesnt_exist", "r");
  fgets(str, 60, f);
  fclose(f);
}

void nocrash_fgets() {
  FILE* f;
  char str[60];

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fgets(str, 60, f);
    fclose(f);
  }
}

void crash_rewind() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  rewind(f);
  fclose(f);
}

void nocrash_rewind() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    rewind(f);
    fclose(f);
  }
}

void crash_fileno() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fileno(f);
  fclose(f);
}

void nocrash_fileno() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fileno(f);
    fclose(f);
  }
}

void crash_clearerr() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  clearerr(f);
  fclose(f);
}

void nocrash_clearerr() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    clearerr(f);
    fclose(f);
  }
}

void crash_ferror() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  ferror(f);
  fclose(f);
}

void nocrash_ferror() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    ferror(f);
    fclose(f);
  }
}

void crash_feof() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  feof(f);
  fclose(f);
}

void nocrash_feof() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    feof(f);
    fclose(f);
  }
}

void crash_fprintf() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fprintf(f, "blablabla\n");
  fclose(f);
}

void nocrash_fprintf() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fprintf(f, "blablabla\n");
    fclose(f);
  }
}

void crash_vfprintf() {
  FILE* f;
  va_list arg;

  f = fopen("this_file_doesnt_exist", "r");
  vfprintf(f, "blablabla\n", arg);
  fclose(f);
}

void nocrash_vfprintf() {
  FILE* f;
  va_list arg;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    vfprintf(f, "blablabla\n", arg);
    fclose(f);
  }
}

void crash_fgetpos() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  fgetpos(f, &position);
  fclose(f);
}

void nocrash_fgetpos() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fgetpos(f, &position);
    fclose(f);
  }
}

void crash_fsetpos() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  fsetpos(f, &position);
  fclose(f);
}

void nocrash_fsetpos() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fsetpos(f, &position);
    fclose(f);
  }
}
