/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <dirent.h>
#include <stdarg.h>
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

void no_fopen_check_fclose_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fclose(f);
}

void fopen_check_fclose_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fclose(f);
  }
}

void fclose_null_bad() { fclose(NULL); }

int fclose_on_cleanup_path_bad(char* buf, int size) {
  int ret = -1;
  FILE* f = fopen("this_file_doesnt_exist", "r");
  if (f == NULL) {
    goto out;
  }
  if (fgets(buf, size, f) == NULL) {
    goto out;
  }
  ret = 0;
out:
  fclose(f);
  return ret;
}

void fclose_twice_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fclose(f);
    fclose(f);
  }
}

void fclose_wrapper(FILE* f) { fclose(f); }

void call_fclose_wrapper_null_bad() { fclose_wrapper(NULL); }

void no_fopen_check_fclose_wrapper_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fclose_wrapper(f);
}

void fopen_check_fclose_wrapper_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fclose_wrapper(f);
  }
}

void no_popen_check_pclose_bad() {
  FILE* f;

  f = popen("ls", "r");
  pclose(f);
}

void popen_check_pclose_ok() {
  FILE* f;

  f = popen("ls", "r");
  if (f) {
    pclose(f);
  }
}

void pclose_null_bad() { pclose(NULL); }

int fclose_param_compared_to_null_bad(FILE* f) {
  int ret = 0;
  if (f == NULL) {
    ret = -1;
  }
  fclose(f);
  return ret;
}

void no_tmpfile_check_fclose_bad() {
  FILE* f = tmpfile();
  fclose(f);
}

void no_fdopen_check_fclose_bad(int fd) {
  FILE* f = fdopen(fd, "r");
  fclose(f);
}

void error(int status, int errnum, const char* format, ...);

// error() exits when its status is not zero, but Pulse does not model it
void FP_fopen_check_error_fclose_ok(const char* path) {
  FILE* f = fopen(path, "r");
  if (f == NULL) {
    error(EXIT_FAILURE, 0, "cannot open %s", path);
  }
  fclose(f);
}

void no_opendir_check_closedir_bad() {
  DIR* d;

  d = opendir("this_dir_doesnt_exist");
  closedir(d);
}

void closedir_null_bad() { closedir(NULL); }

void no_fopen_check_fscanf_bad() {
  FILE* f;
  int i;

  f = fopen("this_file_doesnt_exist", "r");
  fscanf(f, "%d", &i);
  fclose(f);
}

void fopen_check_fscanf_ok() {
  FILE* f;
  int i;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fscanf(f, "%d", &i);
    fclose(f);
  }
}

void no_fopen_check_vfscanf_bad(va_list args) {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  vfscanf(f, "%d", args);
  fclose(f);
}

void fopen_check_vfscanf_ok(va_list args) {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    vfscanf(f, "%d", args);
    fclose(f);
  }
}

void no_fopen_check_getline_bad() {
  FILE* f;
  char* line = NULL;
  size_t n = 0;

  f = fopen("this_file_doesnt_exist", "r");
  getline(&line, &n, f);
  free(line);
  fclose(f);
}

void fopen_check_getline_ok() {
  FILE* f;
  char* line = NULL;
  size_t n = 0;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    getline(&line, &n, f);
    free(line);
    fclose(f);
  }
}

void no_fopen_check_getdelim_bad() {
  FILE* f;
  char* line = NULL;
  size_t n = 0;

  f = fopen("this_file_doesnt_exist", "r");
  getdelim(&line, &n, ',', f);
  free(line);
  fclose(f);
}

void fopen_check_getdelim_ok() {
  FILE* f;
  char* line = NULL;
  size_t n = 0;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    getdelim(&line, &n, ',', f);
    free(line);
    fclose(f);
  }
}

void no_fopen_check_fseeko_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  fseeko(f, 7, SEEK_SET);
  fclose(f);
}

void fopen_check_fseeko_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    fseeko(f, 7, SEEK_SET);
    fclose(f);
  }
}

void no_fopen_check_ftello_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  ftello(f);
  fclose(f);
}

void fopen_check_ftello_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    ftello(f);
    fclose(f);
  }
}

void no_fopen_check_setbuf_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  setbuf(f, NULL);
  fclose(f);
}

void fopen_check_setbuf_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    setbuf(f, NULL);
    fclose(f);
  }
}

void no_fopen_check_setvbuf_bad() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  setvbuf(f, NULL, _IONBF, 0);
  fclose(f);
}

void fopen_check_setvbuf_ok() {
  FILE* f;

  f = fopen("this_file_doesnt_exist", "r");
  if (f) {
    setvbuf(f, NULL, _IONBF, 0);
    fclose(f);
  }
}

// flushes all the output streams
void fflush_null_ok() { fflush(NULL); }

int fsctl(const char* path,
          unsigned long request,
          void* data,
          unsigned int options);

void fsctl_null_path_bad() { fsctl(NULL, 0, NULL, 0); }

void fsctl_null_data_ok(const char* path) { fsctl(path, 0, NULL, 0); }

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

void fscanf_after_fclose_bad(FILE* f) {
  int i;

  fclose(f);
  fscanf(f, "%d", &i);
}

void getline_after_fclose_bad() {
  char* line = NULL;
  size_t n = 0;
  FILE* f = fopen("some_file", "r");
  if (!f) {
    return;
  }
  fclose(f);
  getline(&line, &n, f);
  free(line);
}

void fflush_after_fclose_bad(FILE* f) {
  fclose(f);
  fflush(f);
}

int fscanf_error_no_fclose_bad() {
  int i;
  FILE* f = fopen("some_file", "r");
  if (!f) {
    return -1;
  }
  if (fscanf(f, "%d", &i) != 1) {
    return -1;
  }
  fclose(f);
  return i;
}

int fscanf_fclose_ok() {
  int i;
  FILE* f = fopen("some_file", "r");
  if (!f) {
    return -1;
  }
  int n = fscanf(f, "%d", &i);
  fclose(f);
  if (n != 1) {
    return -1;
  }
  return i;
}

int fscanf_returned_no_fclose_bad(int* i) {
  FILE* f = fopen("some_file", "r");
  if (!f) {
    return -1;
  }
  return fscanf(f, "%d", i);
}

void fflush_no_fclose_bad() {
  FILE* f = fopen("some_file", "w");
  if (f) {
    fputs("blablabla", f);
    fflush(f);
  }
}

void setvbuf_no_fclose_bad() {
  FILE* f = fopen("some_file", "r");
  if (f) {
    setvbuf(f, NULL, _IONBF, 0);
  }
}

FILE* global_stream;

// the stream uses the buffer until it is closed
void setvbuf_malloc_buffer_ok() {
  global_stream = fopen("some_file", "w");
  if (global_stream) {
    char* buf = malloc(BUFSIZ);
    setvbuf(global_stream, buf, _IOFBF, BUFSIZ);
  }
}

void set_stream_buffer(FILE* f, char* buf) { setvbuf(f, buf, _IOFBF, BUFSIZ); }

void setvbuf_in_callee_malloc_buffer_ok() {
  global_stream = fopen("some_file", "w");
  if (global_stream) {
    char* buf = malloc(BUFSIZ);
    set_stream_buffer(global_stream, buf);
  }
}

// the buffer has to be freed even if getline fails
int getline_failure_no_free_bad(FILE* f) {
  char* line = NULL;
  size_t n = 0;
  if (getline(&line, &n, f) == -1) {
    return -1;
  }
  free(line);
  return 0;
}

int getline_loop_ok(FILE* f) {
  char* line = NULL;
  size_t n = 0;
  int count = 0;
  while (getline(&line, &n, f) != -1) {
    count += line[0];
  }
  free(line);
  return count;
}

void getline_caller_buffer_ok(FILE* f) {
  size_t n = 16;
  char* line = malloc(n);
  if (!line) {
    return;
  }
  getline(&line, &n, f);
  free(line);
}

ssize_t read_line(char** line, size_t* n, FILE* f) {
  return getline(line, n, f);
}

int read_line_loop_ok(FILE* f) {
  char* line = NULL;
  size_t n = 0;
  int count = 0;
  while (read_line(&line, &n, f) != -1) {
    count += line[0];
  }
  free(line);
  return count;
}

void read_line_caller_buffer_ok(FILE* f) {
  size_t n = 16;
  char* line = malloc(n);
  if (!line) {
    return;
  }
  read_line(&line, &n, f);
  free(line);
}

ssize_t read_line_no_free_bad(FILE* f) {
  char* line = NULL;
  size_t n = 0;
  return read_line(&line, &n, f);
}

void fscanf_propagate_taint_bad() {
  char* tainted = string_source();
  FILE* file = fopen(tainted, "r");
  if (!file) {
    return;
  }
  int i;
  if (fscanf(file, "%d", &i) == 1) {
    sink_int(i);
  }
  fclose(file);
}

void getline_propagate_taint_bad() {
  char* tainted = string_source();
  FILE* file = fopen(tainted, "r");
  if (!file) {
    return;
  }
  char* line = NULL;
  size_t n = 0;
  if (getline(&line, &n, file) != -1) {
    sink_string(line);
  }
  free(line);
  fclose(file);
}
