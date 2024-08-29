/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>
#include <stdlib.h>

void no_fopen_check_getc_bad() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  i = getc(f);
  printf("i =%i\n", i);
  fclose(f);
}

void fopen_check_getc_ok() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    i = getc(f);
    printf("i =%i\n", i);
    fclose(f);
  }
}

void fopen_no_fclose_bad() {
  FILE* f;
  int i;
  f = fopen("some_file", "r");
  if (f) {
    i = getc(f);
    printf("i =%i\n", i);
  }
}

void no_fopen_check_fgetc_bad() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  i = fgetc(f);
  printf("i =%i\n", i);
  fclose(f);
}

void fopen_check_fgetc_ok() {
  FILE* f;
  int i;
  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    i = fgetc(f);
    printf("i =%i\n", i);
    fclose(f);
  }
}

void no_fopen_check_ungetc_bad() {
  FILE* f;
  f = fopen("this_file_doesnt_exist", "r");
  int i = ungetc(10, f);
  fclose(f);
}

void fopen_check_ungetc_ok() {
  FILE* f;
  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    int i = ungetc(10, f);
    fclose(f);
  }
}

void no_fopen_check_fputs_bad() {
  FILE* f;
  f = fopen("this_file_doesnt_exist", "r");
  fputs("blablabla", f);
  fclose(f);
}

void fopen_check_fputs_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fputs("blablabla", f);
    fclose(f);
  }
}

void no_fopen_check_fputc_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fputc(42, f);
  fclose(f);
}

void fopen_check_fputc_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fputc(42, f);
    fclose(f);
  }
}

void no_fopen_check_putc_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  putc(42, f);
  fclose(f);
}

void fopen_check_putc_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    putc(42, f);
    fclose(f);
  }
}

void no_fopen_check_fseek_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fseek(f, 7, SEEK_SET);
  fclose(f);
}

void fopen_check_fseek_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fseek(f, 7, SEEK_SET);
    fclose(f);
  }
}

void no_fopen_check_ftell_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  ftell(f);
  fclose(f);
}

void fopen_check_ftell_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    ftell(f);
    fclose(f);
  }
}

void no_fopen_check_fgets_bad() {
  FILE* f;
  char str[60];

  f = fopen("this_file_doesnt_exist", "r");
  fgets(str, 60, f);
  fclose(f);
}

void fopen_check_fgets_ok() {
  FILE* f;
  char str[60];

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fgets(str, 60, f);
    fclose(f);
  }
}

void no_fopen_check_rewind_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  rewind(f);
  fclose(f);
}

void fopen_check_rewind_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    rewind(f);
    fclose(f);
  }
}

void no_fopen_check_fileno_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fileno(f);
  fclose(f);
}

void fopen_check_fileno_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fileno(f);
    fclose(f);
  }
}

void no_fopen_check_clearerr_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  clearerr(f);
  fclose(f);
}

void fopen_check_clearerr_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    clearerr(f);
    fclose(f);
  }
}

void no_fopen_check_ferror_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  ferror(f);
  fclose(f);
}

void fopen_check_ferror_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    ferror(f);
    fclose(f);
  }
}

void no_fopen_check_feof_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  feof(f);
  fclose(f);
}

void fopen_check_feof_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    feof(f);
    fclose(f);
  }
}

void no_fopen_check_fprintf_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fprintf(f, "blablabla\n");
  fclose(f);
}

void fopen_check_fprintf_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fprintf(f, "blablabla\n");
    fclose(f);
  }
}

/* NOTE: Temporarily commented out since these tests make different results on
   macos arm machine.

void no_fopen_check_vfprintf_bad() {
  FILE* f;
  va_list arg;

  f = fopen("this_file_doesnt_exist", "r");
  vfprintf(f, "blablabla\n", arg);
  fclose(f);
}

void fopen_check_vfprintf_ok() {
  FILE* f;
  va_list arg;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    vfprintf(f, "blablabla\n", arg);
    fclose(f);
  }
} */

void no_fopen_check_fgetpos_bad() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  fgetpos(f, &position);
  fclose(f);
}

void fopen_check_fgetpos_ok() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fgetpos(f, &position);
    fclose(f);
  }
}

void no_fopen_check_fsetpos_bad() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  fsetpos(f, &position);
  fclose(f);
}

void fopen_check_fsetpos_ok() {
  FILE* f;
  fpos_t position;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fsetpos(f, &position);
    fclose(f);
  }
}

char* string_source();
void sink_string(char* s);
void sink_int(int c);

// excepting 3 taint flows
void file_operations_propagate_taint_bad() {
  char* tainted = string_source();
  FILE* file = fopen(tainted, "r");
  if (!file) {
    return;
  }
  char s[256];
  char* t = fgets(s, 256, file);
  sink_string(t);
  sink_int(fgetc(file));
  sink_int(getc(file));
  sink_int(fileno(file)); // benign
  fclose(file);
}

void fprintf_propagate_taint_bad() {
  char* tainted = string_source();
  FILE* file = fopen("some_file", "r");
  if (!file) {
    return;
  }
  fprintf(file, "%s", tainted);
  sink_int(getc(file));
  fclose(file);
}

void fputs_propagate_taint_bad() {
  char* tainted = string_source();
  FILE* file = fopen("some_file", "r");
  if (!file) {
    return;
  }
  fputs(tainted, file);
  sink_int(getc(file));
  fclose(file);
}

void FN_fputc_propagate_taint_bad() {
  char* tainted = string_source();
  FILE* file = fopen("some_file", "r");
  if (!file) {
    return;
  }
  fputc(file, tainted[42]);
  sink_int(getc(file));
  fclose(file);
}
