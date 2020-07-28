/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Basic modelling of some libc functions

// prevent _FORTIFY_SOURCE from changing some function prototypes
// https://securityblog.redhat.com/2014/03/26/fortify-and-you/
#ifdef _FORTIFY_SOURCE
#undef _FORTIFY_SOURCE
#endif
#define _FORTIFY_SOURCE 0

#ifdef __APPLE__ // disable block instructions on mac
#ifdef __BLOCKS__
#undef __BLOCKS__
#endif
#ifdef _POSIX_C_SOURCE
#undef _POSIX_C_SOURCE
#endif
#endif

#include "infer_builtins.h"

// use c++ headers if in C++ mode - they are mostly same as C headers,
// but there are some subtle differences from time to time. For example,
// 'getc' may be defined as macro in stdio.h, and a function in cstdio
#ifdef __cplusplus
#include <climits>
#include <clocale>
#include <csetjmp>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <csignal>
#else
#include <limits.h>
#include <locale.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#endif

#include <dirent.h>
#include <errno.h>
#include <pwd.h>
#include <pthread.h>
#include <sys/shm.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __APPLE__ // includes for statfs are system-dependent
#include <sys/mount.h>
#include <sys/param.h>
#else
#include <sys/statfs.h>
#endif

#ifndef __APPLE__
/* struct __dirstream is abstract on Linux, at least on Debian 8.0
   we need it to be concrete to model closedir (5) */
/* dummy __dirstream structure */
struct __dirstream {
  int fd;
};
#endif

// this condition checks whether to use C++ const-overloads of C functions
// for example strchr.
#if defined(__CORRECT_ISO_CPP_STRING_H_PROTO) || \
    defined(_LIBCPP_PREFERRED_OVERLOAD)
#define INFER_USE_CPP_CONST_OVERLOAD
#endif

// modelling of errno
// errno expands to different function calls on mac or other systems
// the function call returns the address of a global variable called "errno"
#ifdef errno
// errno may be defined as a macro to be a per-thread variable
#undef errno
#endif
extern int errno;
#ifdef __APPLE__
#define __ERRNO_FUN_NAME __error
#else
#define __ERRNO_FUN_NAME() \
  __errno_location(void) __THROW __attribute__((__const__))
#endif
int* __ERRNO_FUN_NAME() { return &errno; }

// the strings s1 and s2 need to be allocated
// check that s2 fits inside s1
char* strcpy(char* s1, const char* s2) {
  int size1;
  int size2;
  __infer_set_flag("ignore_return",
                   ""); // no warnings if the return value is ignored
  __require_allocated_array(s1);
  size1 = __get_array_length(s1);
  __require_allocated_array(s2);
  size2 = __get_array_length(s2);
  INFER_EXCLUDE_CONDITION(size2 > size1);
  return s1;
}

// the string s must be allocated; return the result of malloc with the same
// size
char* strdup(const char* s) {
  int size;
  __require_allocated_array(s);
  size = __get_array_length(s);
  return (char*)malloc(size);
}

// the strings s1 and s2 need to be allocated
// check that s2 fits inside s1
char* strcat(char* s1, const char* s2) {
  int size1;
  int size2;
  __require_allocated_array(s1);
  size1 = __get_array_length(s1);
  __require_allocated_array(s2);
  size2 = __get_array_length(s2);
  INFER_EXCLUDE_CONDITION(size2 > size1);
  return s1;
}

// C++ has two versions of strchr:
// http://www.cplusplus.com/reference/cstring/strchr/
// C has one version of strchr/strrchr with different signature

// The string s must be allocated
// nondeterministically return 0 or a pointer inside the buffer
#ifndef INFER_USE_CPP_CONST_OVERLOAD
char* strchr(const char* s, int c) {
#else
// This overload is commented out on purpose.
// Standard headers define both functions with same __asm symbol which
// means they cannot be both defined. On the other hand, since both of them
// have same __asm symbol, mangling will be the same and infer will have
// specs for both functions (they both will have the same name)
// NOTE: this was tested on couple of systems, it may not be always true.
// const char* strchr(const char* s, int c) throw() { return strchr((char*)s,
// c); }

char* strchr(char* s, int c) throw() {
#endif
  int size;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();

  __require_allocated_array(s);
  size = __get_array_length(s);
  if (nondet)
    return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= size);
  return (char*)s + offset;
}

// modelled like strchr
#ifndef INFER_USE_CPP_CONST_OVERLOAD
char* strrchr(const char* s, int c) { return strchr(s, c); }
#else
// This overload is commented out on purpose. Look at strchr() for more info.
/*const char* strrchr(const char* s, int c) throw() {
  return strchr((char*)s, c);
}*/

char* strrchr(char* s, int c) throw() { return strchr(s, c); }
#endif

// s1 and s2 must be allocated.
// return a non-deterministic integer
int strcmp(const char* s1, const char* s2) {
  int res;
  res = __infer_nondet_int();

  __require_allocated_array(s1);
  __require_allocated_array(s2);
  return res;
}

// the string s must be allocated
// return the size of the buffer - 1
size_t strlen(const char* s) {
  int size;
  __require_allocated_array(s);
  size = __get_array_length(s);
  return size - 1;
}

// s must be allocated
// return s
char* strlwr(char* s) {
  __require_allocated_array(s);
  return s;
}

// s1 and s2 must be allocated
// n should not be greater than the size of s1 or s2
// return s1
char* strncat(char* s1, const char* s2, size_t n) {
  int size_s1;
  int size_s2;
  __require_allocated_array(s1);
  size_s1 = __get_array_length(s1);
  __require_allocated_array(s2);
  size_s2 = __get_array_length(s2);
  INFER_EXCLUDE_CONDITION((n > size_s1) || (n > size_s2));
  return s1;
}

// s1 and s2 must be allocated
// n should not be greater than the size of s1 or s2
// return a non-deterministic integer
int strncmp(const char* s1, const char* s2, size_t n) {
  int size_s1;
  int size_s2;
  int res;
  res = __infer_nondet_int();
  __require_allocated_array(s1);
  size_s1 = __get_array_length(s1);
  __require_allocated_array(s2);
  size_s2 = __get_array_length(s2);
  INFER_EXCLUDE_CONDITION((n > size_s1) || (n > size_s2));
  return res;
}

// the strings s1 and s2 need to be allocated
// check that n characters fit in s1 (because even if s2 is shorter than n, null
// characters are appended to s1)
char* strncpy(char* s1, const char* s2, size_t n) {
  int size1;
  __infer_set_flag("ignore_return",
                   ""); // no warnings if the return value is ignored
  __require_allocated_array(s1);
  size1 = __get_array_length(s1);
  __require_allocated_array(s2);
  INFER_EXCLUDE_CONDITION(n > size1);
  return s1;
}

#ifndef INFER_USE_CPP_CONST_OVERLOAD
char* strpbrk(const char* s1, const char* s2) {
#else
// This overload is commented out on purpose. Look at strchr() for more info.
/*const char* strpbrk(const char* s1, const char* s2) throw() {
  return strpbrk((char*)s1, s2);
}*/

char* strpbrk(char* s1, const char* s2) throw() {
#endif
  int size1;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();
  __require_allocated_array(s1);
  size1 = __get_array_length(s1);
  __require_allocated_array(s2);
  if (nondet)
    return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= size1);
  return (char*)s1 + offset;
}

// s1 and s2 must be allocated.
// return an integer between 0 ans the size of s1
size_t strspn(const char* s1, const char* s2) {
  int size_s1;
  int res;
  res = __infer_nondet_int();
  __require_allocated_array(s1);
  size_s1 = __get_array_length(s1);
  __require_allocated_array(s2);
  INFER_EXCLUDE_CONDITION(res < 0 || res > size_s1);
  return res;
}

#ifndef INFER_USE_CPP_CONST_OVERLOAD
char* strstr(const char* s1, const char* s2) {
#else
// This overload is commented out on purpose. Look at strchr() for more info.
/*const char* strstr(const char* s1, const char* s2) throw() {
  return strstr((char*)s1, s2);
}*/

char* strstr(char* s1, const char* s2) throw() {
#endif
  int size1, size2;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();
  __require_allocated_array(s1);
  size1 = __get_array_length(s1);
  __require_allocated_array(s2);
  if (nondet)
    return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= size1);
  return (char*)s1 + offset;
}

// modeled using strtoul
double strtod(const char* str, char** endptr) {
  return (double)strtoul(str, endptr, 0);
}

// modeled like strtoul
long strtol(const char* str, char** endptr, int base) {
  return (long)strtoul(str, endptr, base);
}

// the string s must be allocated
// assign to endptr a pointer somewhere inside the string str
// result is nondeterministic
unsigned long strtoul(const char* str, char** endptr, int base) {
  int size;
  int offset;
  int res;
  __require_allocated_array(str);
  size = __get_array_length(str);
  offset = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= size);
  if (endptr)
    *endptr = (char*)(str + offset);
  res = __infer_nondet_int();
  int errno_val = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(errno_val < 0);
  errno = errno_val;
  return res;
}

// s must be allocated
// return s
char* strupr(char* s) {
  __require_allocated_array(s);
  return s;
}

// the array s must be allocated
// n should not be greater than the size of s
// nondeterministically return 0 or a pointer within the first n elements of s
#ifndef INFER_USE_CPP_CONST_OVERLOAD
void* memchr(const void* s, int c, size_t n) {
#else
// This overload is commented out on purpose. Look at strchr() for more info.
// const void* memchr(const void* s, int c, size_t n) throw() {
//  return memchr((void*)s, c, n);
//}

void* memchr(void* s, int c, size_t n) throw() {
#endif
  int size;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();
  __require_allocated_array(s);
  size = __get_array_length(s);
  INFER_EXCLUDE_CONDITION(n > size);
  if (nondet)
    return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= n);
  return (char*)s + offset;
}

// s1 and s2 must be allocated
// n should not be greater than the size of s1 or s2
// return a non-deterministic integer
int memcmp(const void* s1, const void* s2, size_t n) {
  int size_s1;
  int size_s2;
  __require_allocated_array(s1);
  size_s1 = __get_array_length(s1);
  __require_allocated_array(s2);
  size_s2 = __get_array_length(s2);
  INFER_EXCLUDE_CONDITION((n > size_s1) || (n > size_s2));
  return __infer_nondet_int();
}

// s1 and s2 must be allocated
// n must be between 0 and the minumum of the sizes of s1 and s2
void* memcpy(void* s1, const void* s2, size_t n) {
  int size_s1;
  int size_s2;
  __infer_set_flag("ignore_return",
                   ""); // no warnings if the return value is ignored
  __require_allocated_array(s1);
  size_s1 = __get_array_length(s1);
  __require_allocated_array(s2);
  size_s2 = __get_array_length(s2);
  INFER_EXCLUDE_CONDITION((n < 0) || (n > size_s1) || (n > size_s2));
  return s1;
}

// modeld using memcpy
void* memmove(void* s1, const void* s2, size_t n) {
  __infer_set_flag("ignore_return",
                   ""); // no warnings if the return value is ignored
  return memcpy(s1, s2, n);
}

// s needs to be allocated
// n should not be greater than the size of s
void* memset(void* s, int c, size_t n) {
  int size_s;
  __infer_set_flag("ignore_return",
                   ""); // no warnings if the return value is ignored
  __require_allocated_array(s);
  size_s = __get_array_length(s);
  INFER_EXCLUDE_CONDITION(n > size_s);
  return s;
}

// return a nondeterministic double
double atof(const char* str) { return __infer_nondet_double(); }

// return a nondeterministic value between INT_MIN and INT_MAX
int atoi(const char* str) {
  int retu = INT_MAX;
  int retl = INT_MIN;
  int ret;
  if (__infer_nondet_int())
    ret = retu;
  else
    ret = retl;
  return ret;
}

// modeled using malloc and set file attribute
FILE* fopen(const char* filename, const char* mode) {
  FILE* ret;
  ret = (FILE*)malloc(sizeof(FILE));
  if (ret)
    __set_file_attribute(ret);
  return ret;
}

// modeled using fopen
FILE* tmpfile(void) { return fopen("foo", ""); }

// use a global variable to model the return value of tmpnam
extern char* _tmpnam_global;
// return NULL, or if s is NULL return a global variable, otherwise check that s
// has size at least L_tmpnam and return s
char* tmpnam(char* s) {
  int success;
  int size;

  success = __infer_nondet_int();
  if (!success)
    return NULL;
  if (s) {
    __require_allocated_array(s);
    size = __get_array_length(s);
    INFER_EXCLUDE_CONDITION(size < L_tmpnam);
    return s;
  } else
    return _tmpnam_global;
}

// nondeterministically return NULL or the original stream
FILE* freopen(const char* __restrict filename,
              const char* __restrict mode,
              FILE* __restrict stream) {
  int n;
  n = __infer_nondet_int();
  if (n)
    return NULL;
  else
    return stream;
}

// modeled using free, can return EOF
int fclose(FILE* stream) {
  int n;
  free(stream);
  n = __infer_nondet_int();
  if (n > 0)
    return 0;
  else
    return EOF;
}

// modeled using malloc and set file attribute
// return the allocated ptr - 1 if malloc succeeds, otherwise return -1
int open(const char* path, int oflag, ...) {
  int* ret = (int*)malloc(sizeof(int));
  if (ret) {
    __set_file_attribute(ret);
    INFER_EXCLUDE_CONDITION(ret < (int*)1); // force result to be > 0
    return (size_t)ret;
  }
  return -1;
}

// modeled using free, can return -1
int close(int fildes) {
  int n;
  if (fildes != -1)
    free((int*)(long)fildes);
  n = __infer_nondet_int();
  if (n > 0)
    return 0;
  else
    return -1;
}

// modeled as close followed by fopen
FILE* fdopen(int fildes, const char* mode) {
  close(fildes);
  return fopen("foo", mode);
}

// modeled using fdopen
FILE* gzdopen(int fildes, const char* mode) { return fdopen(fildes, mode); }

// return nonteterministically 0 or -1
// requires stream to be allocated
int fseek(FILE* stream, long int offset, int whence) {
  int n;
  FILE tmp;
  tmp = *stream;
  n = __infer_nondet_int();
  if (n)
    return 0;
  else
    return -1;
}

// return nondeterministically a nonnegative value or -1
// requires stream to be allocated
long int ftell(FILE* stream) {
  int n;
  FILE tmp;
  tmp = *stream;
  n = __infer_nondet_int();
  if (n >= 0)
    return n;
  else
    return -1;
}

// on success return str otherwise null
// requires stream to be allocated
char* fgets(char* str, int num, FILE* stream) {
  int n;
  int size1;
  FILE tmp;

  tmp = *stream;
  n = __infer_nondet_int();

  if (n > 0) {
    __require_allocated_array(str);
    size1 = __get_array_length(str);
    INFER_EXCLUDE_CONDITION(num > size1);
    return str;
  } else
    return NULL;
}

// string s must be allocated; return nondeterministically s or NULL
char* gets(char* s) {
  int n;

  __require_allocated_array(s);
  n = __infer_nondet_int();
  if (n)
    return s;
  else
    return NULL;
}

// str must be allocated, return a nondeterministic value
int puts(const char* str) {
  __require_allocated_array(str);
  return __infer_nondet_int();
}

// modeled using puts
// require the stream to be allocated
int fputs(const char* str, FILE* stream) {
  FILE tmp;
  tmp = *stream;
  return puts(str);
}

// return a nondeterministic value
// requires stream to be allocated
int getc(FILE* stream) {
  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

// return a nondeterministic value
// requires stream to be allocated
int fgetc(FILE* stream) {
  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

// return nondeterministically c or EOF
// requires stream to be allocated
int ungetc(int c, FILE* stream) {
  int n;
  FILE tmp;
  tmp = *stream;

  n = __infer_nondet_int();
  if (n)
    return c;
  else
    return EOF;
}

// modeled like putc
// requires stream to be allocated
int fputc(int c, FILE* stream) {
  FILE tmp;
  tmp = *stream;
  return putc(c, stream);
}

// on success return buffer otherwise null
char* getcwd(char* buffer, size_t size) {
  int n;
  int size_buf;
  char* result;

  if (buffer == NULL) {
    if (size == 0) {
      n = __infer_nondet_int();
    } else {
      n = size;
    }
    return (char*)malloc(n);
  } else {
    n = __infer_nondet_int();

    if (n > 0) {
      __require_allocated_array(buffer);
      size_buf = __get_array_length(buffer);
      INFER_EXCLUDE_CONDITION(size > size_buf);
      return buffer;
    } else {
      return NULL;
    }
  }
}

// return nonteterministically 0 or -1
int rename(const char* old, const char* new_) {
  int n;

  n = __infer_nondet_int();
  if (n)
    return 0;
  else
    return -1;
}

// modeled as skip
// requires stream to be allocated
void rewind(FILE* stream) {
  FILE tmp;
  tmp = *stream;
}

// modeled as exit()
void longjmp(jmp_buf env, int val) { exit(0); }

// modeled as skip.
// If sleep() returns because the requested time has elapsed,
// the value returned shall be 0. If sleep() returns due to delivery of a
// signal,
// the return value shall be the "unslept" amount (the requested time minus the
// time actually slept) in seconds.
unsigned sleep(unsigned seconds) {
  int n;
  n = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(n < 0);
  return n;
}

#ifdef __CYGWIN__ // define __WAIT_STATUS as int * on cygwin
#define __WAIT_STATUS int*
#endif
#ifdef __APPLE__ // define __WAIT_STATUS as int * on mac
#define __WAIT_STATUS int*
#endif

// glibc 2.24 did away with 'union wait' and replaced it with int*.
// Hence, we re-define __WAIT_STATUS if glibc is >= 2.24.
#ifndef __GLIBC_PREREQ
#define __GLIBC_PREREQ(x, y) 0
#endif

#if defined(__GLIBC__) && __GLIBC_PREREQ(2, 24)
#define __WAIT_STATUS int*
#endif

// the waiting is modeled as skip. Then the return value is a random value of a
// process or
// -1 in case of an error
pid_t wait(__WAIT_STATUS stat_loc) {
  int n;
  pid_t tmp;
  n = __infer_nondet_int();
  tmp = __infer_nondet_int();
  if (n > 0)
    return tmp;
  else
    return -1;
}

#ifndef __cplusplus
// return a nondeterministic pointer
void (*signal(int sig, void (*func)(int)))(int) {
  void (*res)(int) = __infer_nondet_ptr();
  return res;
};
#endif

// modelled as exit
void pthread_exit(void* value_ptr) { exit(0); };

// INTENDED SEMANTICS: The setitimer() function shall set the timer specified by
// which
// to the value specified in the structure pointed to by value, and if ovalue is
// not a null pointer,
// store the previous value of the timer in the structure pointed to by ovalue.
int setitimer(int which,
              const struct itimerval* __restrict value,
              struct itimerval* __restrict ovalue) {
  int n;
  int tmp;
  n = __infer_nondet_int();
  tmp = __infer_nondet_int();
  // not sure about this assignment.
  // it should somehow change the value of the timer which.
  // But which is just an int, so it looks like this is useless
  which = tmp;
  if (n > 0)
    return 0;
  else
    return -1;
}

// pause returns only when a signal is received
// the return value is always -1
int pause(void) { return -1; }

// modeled with nondeterministic value n
pid_t fork(void) { return __infer_nondet_int(); }

// allocate memory directly, and return -1 if malloc fails.
int shmget(key_t key, size_t size, int shmflg) {
  void* res = malloc(size);
  if (!res)
    return -1;
  return (long)res;
}

// simply return the first parameter
void* shmat(int shmid, const void* shmaddr, int shmflg) {
  return (void*)(long)shmid;
}

// return a non-deterministic value
int shmdt(const void* shmaddr) { return __infer_nondet_int(); }

// if the command is IPC_RMID free the first parameter; return a
// non-deterministic value
int shmctl(int shmid, int cmd, struct shmid_ds* buf) {
  int n;
  void* shmaddr;
  n = __infer_nondet_int();
  shmaddr = (void*)(long)shmid;
  if (cmd == IPC_RMID)
    free(shmaddr);
  return n;
}

void* realloc(void* ptr, size_t size) {
  if (ptr == 0) { // if ptr in NULL, behave as malloc
    return malloc(size);
  }
  int can_enlarge;
  __require_allocated_array(ptr);
  can_enlarge = __infer_nondet_int(); // nondeterministically choose whether the
  // current block can be enlarged
  if (can_enlarge) {
    __set_array_length(ptr, size); // enlarge the block
    return ptr;
  }
  int* newblock = (int*)malloc(size);
  if (newblock) {
    free(ptr);
    return newblock;
  } else { // if new allocation fails, do not free the old block
    return newblock;
  }
}

// modelled as a call to malloc
void* calloc(size_t nmemb, size_t size) { return malloc(nmemb * size); }

// character functions from ctype.h
// all modelled as returning a nondeterministic value
int isalnum(int x) { return __infer_nondet_int(); }
int isalpha(int x) { return __infer_nondet_int(); }
int isblank(int x) { return __infer_nondet_int(); }
int iscntrl(int x) { return __infer_nondet_int(); }
int isdigit(int x) { return __infer_nondet_int(); }
int isgraph(int x) { return __infer_nondet_int(); }
int islower(int x) { return __infer_nondet_int(); }
int isprint(int x) { return __infer_nondet_int(); }
int ispunct(int x) { return __infer_nondet_int(); }
int isspace(int x) { return __infer_nondet_int(); }
int isupper(int x) { return __infer_nondet_int(); }
int isxdigit(int x) { return __infer_nondet_int(); }
int tolower(int x) { return __infer_nondet_int(); }
int toupper(int x) { return __infer_nondet_int(); }
int isascii(int x) { return __infer_nondet_int(); }
int toascii(int x) { return __infer_nondet_int(); }

// modeled as skip. n>0 models success.
// Upon successful completion a non-negative integer, namely the file
// descriptor, shall be returned;
// otherwise, -1 shall be returned.
int dup(int fildes) {
  int n;
  n = __infer_nondet_int();
  if (n > 0)
    return fildes;
  else
    return -1;
};

// modeled as skip.
// The getuid() function shall always be successful and no return value is
// reserved to indicate the error.
uid_t getuid(void) { return __infer_nondet_int(); };

// modeled as skip
// The getpid() function shall always be successful and no return value is
// reserved to indicate an error.
pid_t getpid(void) { return __infer_nondet_int(); };

// modeled as skip.
// success is modeled by n>0, if lseek fails (off_t)-1 should be returned.
off_t lseek(int fildes, off_t offset, int whence) {
  int n;
  off_t tmp;
  n = __infer_nondet_int();
  tmp = __infer_nondet_int();

  if (n > 0)
    return tmp;
  else
    return (off_t)-1;
};

// modeled using malloc and set file attribute
DIR* opendir(const char* dirname) {
  DIR* ret;
  ret = (DIR*)malloc(sizeof(void*));
  if (ret)
    __set_file_attribute(ret);
  return ret;
}

// modeled using free, can return EOF
int closedir(DIR* dirp) {
  DIR tmp = *dirp;
  int n;
  n = __infer_nondet_int();
  free(dirp);
  if (n > 0)
    return 0;
  else
    return EOF;
}

// use a global variable to model the return value of readdir
extern struct dirent* _dirent_global;
// dirp must be allocated
// return 0 or the allocated pointerq _dirent_global, nondeterministically
struct dirent* readdir(DIR* dirp) {
  int nondet;
  struct dirent* ret = _dirent_global;
  __require_allocated_array(dirp);
  nondet = __infer_nondet_int();
  if (nondet)
    return 0;
  return ret;
}

// use a global variable to model the return value of getenv
extern char* _getenv_global;
// string name must be allocated
// return 0 or the allocated string _getenv_global, nondeterministically
char* getenv(const char* name) {
  int nondet;
  __require_allocated_array(name);
  __require_allocated_array(_getenv_global);
  nondet = __infer_nondet_int();
  if (nondet)
    return 0;
  return _getenv_global;
}

// use a global variable to model the return value of setlocale
extern char* _locale_global;
// string locale must be allocated
// return 0 or the allocated string _locale_global, nondeterministically
char* setlocale(int category, const char* locale) {
  int nondet;

  __require_allocated_array(_locale_global);

  if (locale == NULL) {
    return _locale_global;
  }

  __require_allocated_array(locale);
  nondet = __infer_nondet_int();
  if (nondet)
    return 0;
  return _locale_global;
}

// use a global variable to model the return value of localeconv
extern struct lconv* _lconv_global;
// return the allocated pointer _lconv_global
struct lconv* localeconv() {
  int nondet;
  struct lconv tmp;
  tmp = *_lconv_global;
  return _lconv_global;
}

// use a global variable to model the return value of localtime
extern struct tm* _tm_global;
// return nondeterministically NULL or the global variable _tm_global
struct tm* localtime(const time_t* clock) {
  int fail;

  fail = __infer_nondet_int();
  if (fail)
    return NULL;
  else
    return _tm_global;
}

// return nondeterministically NULL or the global variable _tm_global
struct tm* gmtime(const time_t* timer) {
  int fail;

  fail = __infer_nondet_int();
  if (fail)
    return NULL;
  else
    return _tm_global;
}

// use a global variable to model the return value of asctime
extern char* _asctime_global;
// return the global variable _asctime_global
char* asctime(const struct tm* timeptr) { return _asctime_global; }

// modelled using asctime and localtime
char* ctime(const time_t* clock) { return asctime(localtime(clock)); }

// return a nondeterministic double
double difftime(time_t time1, time_t time0) { return __infer_nondet_double(); }

// return a nondeterministic nonnegative value or -1
time_t mktime(struct tm* timeptr) {
  time_t res;
  res = __infer_nondet_time_t();
  INFER_EXCLUDE_CONDITION(res < -1);
  return res;
}

// return a nondeterministic nonnegative value or -1
clock_t clock() {
  clock_t res;
  res = __infer_nondet_clock_t();
  INFER_EXCLUDE_CONDITION(res < -1);
  return res;
}

// return a nondeterministic nonnegative value
size_t strftime(char* __restrict s,
                size_t maxsize,
                const char* __restrict format,
                const struct tm* __restrict timeptr) {
  size_t res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic value of type time_t
time_t time(time_t* tloc) {
  time_t t;
  t = __infer_nondet_time_t();
  if (tloc)
    *tloc = t;
  return t;
}

// use a global variable to model the return value of getpwuid
extern struct passwd* _getpwuid_global;
// return either NULL or the global variable _getpwuid_global
struct passwd* getpwuid(uid_t uid) {
  int found;
  found = __infer_nondet_int();
  if (found)
    return _getpwuid_global;
  else
    return NULL;
}

// use a global variable to model the return value of getpwent
extern struct passwd* _getpwent_global;
// return either NULL or the global variable _getpwent_global
struct passwd* getpwent(void) {
  int found;
  found = __infer_nondet_int();
  if (found)
    return _getpwent_global;
  else
    return NULL;
}

// use a global variable to model the return value of getpwnam
extern struct passwd* _getpwnam_global;
// login must be allocated
// return either NULL or the global variable _getpnam_global
struct passwd* getpwnam(const char* login) {
  int found;
  __require_allocated_array(login);
  found = __infer_nondet_int();
  if (found)
    return _getpwnam_global;
  else
    return NULL;
}

// nondeterministically return 0 (failure) or 1 (success)
int setpassent(int stayopen) {
  int success;
  success = __infer_nondet_int();
  if (success)
    return 1;
  else
    return 0;
}

// modled as skip
void setpwent(void) {}

// modled as skip
void endpwent(void) {}

// use a global variable to model the return value of getlogin
extern char* _getlogin_global;
// string name must be allocated
// return 0 or the allocated string _getlogin_global, nondeterministically
char* getlogin() {
  int size;
  int nondet;
  __require_allocated_array(_getlogin_global);
  nondet = __infer_nondet_int();
  if (nondet)
    return 0;
  return _getlogin_global;
}

int setlogin(const char* name) {
  int success;
  __require_allocated_array(name);
  success = __infer_nondet_int();
  if (success) {
    strcpy(_getlogin_global, name);
    return 0;
  } else
    return -1;
}

// use a global variable to model the return value of getpass
extern char* _getpass_global;
// prompt must be allocated
// return the global variable _getpass_global, which must be allocated
char* getpass(const char* prompt) {
  __require_allocated_array(prompt);
  __require_allocated_array(_getpass_global);
  return _getpass_global;
}

// return a nondeterministic nonnegative integer
int printf(const char* format, ...) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
// requires stream to be allocated
int fprintf(FILE* stream, const char* format, ...) {
  int res;
  FILE tmp;
  tmp = *stream;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int snprintf(char* __restrict s, size_t n, const char* __restrict format, ...) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// s must be allocated
// return a nondeterministic nonnegative integer
int sprintf(char* s, const char* format, ...) {
  int res;
  __require_allocated_array(s);
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
// requires stream to be allocated
int vfprintf(FILE* stream, const char* format, va_list arg) {
  int res;
  FILE tmp;

  tmp = *stream;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int vsprintf(char* s, const char* format, va_list arg) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int vsnprintf(char* s, size_t n, const char* format, va_list arg) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int vprintf(const char* format, va_list arg) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// forces path to be allocated
// return nondeterministically 0 or -1
int utimes(const char* path, const struct timeval times[2]) {
  int success = __infer_nondet_int();

  __require_allocated_array(path);
  // return random result
  return (success > 0) ? 0 : -1;
}

// forces filename  to be allocated
// return nondeterministically 0 or -1
int unlink(const char* filename) {
  int success = __infer_nondet_int();
  __require_allocated_array(filename);
  return (success > 0) ? 0 : -1;
}

// return nondeterministically 0 or -1
int usleep(useconds_t useconds) {
  int success = __infer_nondet_int();
  return (success > 0) ? 0 : -1;
}

// dst and src must be allocated
// return an integer between 0 and size
size_t strlcpy(char* dst, const char* src, size_t size) {
  int size_dst;
  int res;

  res = __infer_nondet_int();

  // force src to be allocated
  __require_allocated_array(src);

  // force dst to be allocated for at least size
  __require_allocated_array(dst);
  size_dst = __get_array_length(dst);
  INFER_EXCLUDE_CONDITION(size > size_dst);

  INFER_EXCLUDE_CONDITION(res > size || res < 0);

  return res;
}

// dst and src must be allocated
// return an integer between 0 and size
size_t strlcat(char* dst, const char* src, size_t size) {
  int size_dst;
  int res;

  res = __infer_nondet_int();

  // force src to be allocated
  __require_allocated_array(src);

  // force dst to be allocated for at least size
  __require_allocated_array(dst);
  size_dst = __get_array_length(dst);
  INFER_EXCLUDE_CONDITION(size > size_dst);

  INFER_EXCLUDE_CONDITION(res > size || res < 0);

  return res;
}

// path must be allocated
// assign nonteterministically to the contents of buf
// return 0 or -1
int statfs(const char* path, struct statfs* buf) {
  int success;

  success = __infer_nondet_int();

  // force path to be allocated
  __require_allocated_array(path);

  struct statfs s; // uninitialized struct statfs
  *buf = s;

  return (success > 0) ? 0 : -1;
}

// path must be allocated
// assign nonteterministically to the contents of buf
// return 0 or -1
int stat(const char* path, struct stat* buf) {
  int success;

  success = __infer_nondet_int();

  // force path to be allocated
  __require_allocated_array(path);

  struct stat s; // uninitialized struct stat
  *buf = s;

  return (success > 0) ? 0 : -1;
}

int remove(const char* path) {
  int success;

  // force path to be allocated
  __require_allocated_array(path);

  success = __infer_nondet_int();
  return (success > 0) ? 0 : -1;
}

char* readline(const char* prompt) {
  char* ret;
  int size;

  // force prompt to be allocated when not null
  if (prompt != NULL) {
    __require_allocated_array(prompt);
  }

  // return random string of positive size
  size = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(size < 0);

  ret = (char*)malloc(sizeof(size));
  return ret;
}

long random(void) {
  long ret;

  ret = __infer_nondet_long_int();
  INFER_EXCLUDE_CONDITION(ret < 0);
  return ret;
}

// requires stream to be allocated
int putc(int c, FILE* stream) {
  int rand;
  FILE tmp;

  tmp = *stream;
  rand = __infer_nondet_int();
  if (rand > 0)
    return c; // success
  else
    return EOF; // failure
}

int access(const char* path, int mode) {
  int success;

  // force path to be allocated
  __require_allocated_array(path);

  // determine return value
  success = __infer_nondet_int();

  return (success > 0) ? 0 : -1;
}

size_t confstr(int name, char* buf, size_t len) {
  int ret;

  // determine return value
  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < 0);

  // buf should be allocated if len is not zero. Otherwise, we want buf=0.
  if (len)
    __require_allocated_array(buf);
  else
    INFER_EXCLUDE_CONDITION(buf != 0);

  return ret;
}

// return a non-deterministic value
// stream is not required to be allocated
int fflush(FILE* stream) { return __infer_nondet_int(); }

int flock(int fd, int operation) {
  int ret;

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

int fsync(int fildes) {
  int ret;

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

int fsctl(const char* path,
          unsigned long request,
          void* data,
          unsigned int options) {
  int ret;

  // forces path and data to be allocated
  __require_allocated_array(path);
  __require_allocated_array(data);

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

int getrusage(int who, struct rusage* r_usage) {
  int ret;

  INFER_EXCLUDE_CONDITION(r_usage == 0);

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

struct tm* localtime_r(const time_t* __restrict timer,
                       struct tm* __restrict result) {
  int success;
  struct tm tmp;

  INFER_EXCLUDE_CONDITION(timer == 0);
  INFER_EXCLUDE_CONDITION(result == 0);

  success = __infer_nondet_int();
  *result = tmp;

  return (success > 0) ? result : 0;
}

int mkdir(const char* filename, mode_t mode) {
  int ret;

  __require_allocated_array(filename);

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

int munmap(void* addr, size_t len) {
  int ret;

  INFER_EXCLUDE_CONDITION(addr == 0);
  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

/*
  pthread_mutex_t model:
  locked = 0 if unlocked, non-zero if locked
  initialized = 1 if initialized, non-one otherwise
*/
typedef struct {
  int locked;
  int initialized;
} __infer_model_pthread_mutex_t;

// returns a nondeterministc value
int pthread_mutex_destroy(pthread_mutex_t* mutex) {
  INFER_EXCLUDE_CONDITION(mutex == 0);
  __infer_model_pthread_mutex_t* model = (__infer_model_pthread_mutex_t*)mutex;
  INFER_EXCLUDE_CONDITION_MSG(model->initialized != 1,
                              "DESTROYING_UNINITIALIZED_MUTEX");
  INFER_EXCLUDE_CONDITION_MSG(model->locked != 0, "DESTROYING_LOCKED_MUTEX");
  model->initialized = -1;
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutex_init(pthread_mutex_t* __restrict mutex,
                       const pthread_mutexattr_t* __restrict attr) {
  INFER_EXCLUDE_CONDITION(mutex == 0);
  __infer_model_pthread_mutex_t* model = (__infer_model_pthread_mutex_t*)mutex;
  INFER_EXCLUDE_CONDITION(model->initialized == 1);
  int ret = __infer_nondet_int();
  if (!ret) {
    model->initialized = 1;
    model->locked = 0;
  }
  return ret;
}

// returns a nondeterministc value
int pthread_mutex_lock(pthread_mutex_t* mutex) {
  INFER_EXCLUDE_CONDITION(mutex == 0);
  __infer_model_pthread_mutex_t* model = (__infer_model_pthread_mutex_t*)mutex;
  INFER_EXCLUDE_CONDITION_MSG(model->initialized != 1,
                              "LOCKING_UNINITIALIZED_MUTEX");
  INFER_EXCLUDE_CONDITION_MSG(model->locked != 0,
                              "LOCKING_ALREADY_LOCKED_MUTEX");
  int ret = __infer_nondet_int();
  if (!ret) {
    model->locked = 1;
  }
  return ret;
}

// returns a nondeterministc value
int pthread_mutex_trylock(pthread_mutex_t* mutex) {
  INFER_EXCLUDE_CONDITION(mutex == 0);
  __infer_model_pthread_mutex_t* model = (__infer_model_pthread_mutex_t*)mutex;
  INFER_EXCLUDE_CONDITION_MSG(model->initialized != 1,
                              "TRYLOCKING_UNINITIALIZED_MUTEX");
  if (model->locked) {
    return EBUSY;
  }
  model->locked = 1;
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutex_unlock(pthread_mutex_t* mutex) {
  INFER_EXCLUDE_CONDITION(mutex == 0);
  __infer_model_pthread_mutex_t* model = (__infer_model_pthread_mutex_t*)mutex;
  INFER_EXCLUDE_CONDITION_MSG(model->initialized != 1,
                              "UNLOCKING_UNINITIALIZED_MUTEX");
  INFER_EXCLUDE_CONDITION_MSG(model->locked == 0, "UNLOCKING_UNLOCKED_MUTEX");
  model->locked = 0;
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutexattr_destroy(pthread_mutexattr_t* attr) {
  INFER_EXCLUDE_CONDITION(attr == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutexattr_init(pthread_mutexattr_t* attr) {
  INFER_EXCLUDE_CONDITION(attr == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutexattr_settype(pthread_mutexattr_t* attr, int type) {
  INFER_EXCLUDE_CONDITION(attr == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value and forces type to be allocated and set its
// content
int pthread_mutexattr_gettype(const pthread_mutexattr_t* attr, int* type) {
  INFER_EXCLUDE_CONDITION(attr == 0);
  *type = __infer_nondet_int();
  return __infer_nondet_int();
}

/*
  pthread_spinlock_t model:
  locked: 0 if unlocked, non-zero if locked
*/
typedef struct {
  int locked;
} __infer_model_pthread_spinlock_t;

#ifdef __APPLE__
typedef __infer_model_pthread_spinlock_t pthread_spinlock_t;
#endif

int pthread_spin_destroy(pthread_spinlock_t* lock) {
  __infer_model_pthread_spinlock_t* model =
      (__infer_model_pthread_spinlock_t*)lock;
  INFER_EXCLUDE_CONDITION_MSG(model->locked != 0, "DESTROYING_LOCKED_SPINLOCK");
  return 0;
}

int pthread_spin_init(pthread_spinlock_t* lock, int pshared) {
  __infer_model_pthread_spinlock_t* model =
      (__infer_model_pthread_spinlock_t*)lock;
  model->locked = 0;
  return 0;
}

int pthread_spin_lock(pthread_spinlock_t* lock) {
  __infer_model_pthread_spinlock_t* model =
      (__infer_model_pthread_spinlock_t*)lock;
  INFER_EXCLUDE_CONDITION_MSG(model->locked != 0,
                              "LOCKING_ALREADY_LOCKED_SPINLOCK");
  model->locked = 1;
  return 0;
}

int pthread_spin_trylock(pthread_spinlock_t* lock) {
  __infer_model_pthread_spinlock_t* model =
      (__infer_model_pthread_spinlock_t*)lock;
  if (model->locked) {
    return EBUSY;
  } else {
    model->locked = 1;
    return 0;
  }
}

int pthread_spin_unlock(pthread_spinlock_t* lock) {
  __infer_model_pthread_spinlock_t* model =
      (__infer_model_pthread_spinlock_t*)lock;
  INFER_EXCLUDE_CONDITION_MSG(model->locked == 0,
                              "UNLOCKING_UNLOCKED_SPINLOCK");
  model->locked = 0;
  return 0;
}

// return a positive non-deterministic number or -1.
// requires stream to be allocated
int fileno(FILE* stream) {
  int ret = __infer_nondet_int();
  FILE tmp;
  tmp = *stream;
  INFER_EXCLUDE_CONDITION(ret < -1 || ret == 0);
  return ret;
}

int fstat(int fildes, struct stat* buf) {
  int ret;
  struct stat s; // uninitialized struct stat
  *buf = s;
  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);
  return ret;
}

// treates as skyp. Return nondeterministically 0 or -1
int futimes(int fildes, const struct timeval times[2]) {
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);
  return ret;
}

#ifdef getchar // cygwin defines getchar as a macro
#undef getchar
#endif
int getchar(void) {
  // randomly produce an error
  int ret = __infer_nondet_int();
  if (ret < 0)
    return EOF;
  else
    return ret;
}

int isatty(int fildes) {
  int ret;

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < 0 || ret > 1);
  return ret;
}

void perror(const char* s) { __require_allocated_array(s); }

int pipe(int fildes[2]) {
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);
  return ret;
}

int raise(int sig) { return __infer_nondet_int(); }

ssize_t read(int fildes, void* buf, size_t nbyte) {
  if (nbyte == 0)
    return 0;
  __require_allocated_array(buf);
  INFER_EXCLUDE_CONDITION(__get_array_length(buf) < nbyte);

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > nbyte);
  return ret;
}

int sigaction(int sig, const struct sigaction* act, struct sigaction* oact) {
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);
  return ret;
}

// return the first argument
// long __builtin_expect(long x, long y) {
//  return x;
//}
// TODO: this function has been disabled because it cannot be compiled with
// LLVM.
// It is normally a builtin that should not be implemented and LLVM complains
// about this

#ifdef clearerr // cygwin defines clearerr as a macro
#undef clearerr
#endif
// modelled as skip
// stream is required to be allocated
void clearerr(FILE* stream) {
  FILE tmp;
  tmp = *stream;
}

#ifdef ferror // cygwin defines ferror as a macro
#undef ferror
#endif
// return a nondeterministic value
// stream required to be allocated
int ferror(FILE* stream) {

  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

#ifdef feof // cygwin defines feof as a macro
#undef feof
#endif
// return a nondeterministic value
// stream required to be allocated
int feof(FILE* stream) {

  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

// write to *pos and return either 0 or -1
// stream is required to be allocated
int fgetpos(FILE* __restrict stream, fpos_t* __restrict pos) {

  int success;
  FILE tmp;
  tmp = *stream;
#ifdef __APPLE__ // fpos_t is a long in MacOS, but a struct in Linux.
  *pos = __infer_nondet_long_int();
#else
  pos->__pos = __infer_nondet_long_int();
#endif
  success = __infer_nondet_int();
  if (success)
    return 0;
  else
    return -1;
}

// read from *pos and return either 0 or -1
// stream is required to be allocated
int fsetpos(FILE* stream, const fpos_t* pos) {

  int success;
  FILE tmp;
  tmp = *stream;
  fpos_t t;
  t = *pos;
  success = __infer_nondet_int();
  if (success)
    return 0;
  else
    return -1;
}

// return a value between 0 and nmemb
size_t fread(void* __restrict ptr,
             size_t size,
             size_t nmemb,
             FILE* __restrict stream) {
  size_t res;
  res = __infer_nondet_size_t();
  if (size == 0 || nmemb == 0)
    return 0;
  INFER_EXCLUDE_CONDITION(res < 0 || res > nmemb);
  return res;
}

// return a value between 0 and nmemb
size_t fwrite(const void* __restrict ptr,
              size_t size,
              size_t nmemb,
              FILE* __restrict stream) {
  size_t res;
  res = __infer_nondet_size_t();
  if (size == 0 || nmemb == 0)
    return 0;
  INFER_EXCLUDE_CONDITION(res < 0 || res > nmemb);
  return res;
}

size_t strcspn(const char* s1, const char* s2) {
  __require_allocated_array(s1);
  __require_allocated_array(s2);

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < 0);
  return ret;
}

// There is non-standard version of strerr_r:
// http://linux.die.net/man/3/strerror_r
// It's not modelled right now
#if defined __APPLE__ || (defined __USE_XOPEN2K && !defined __USE_GNU)
int strerror_r(int errnum, char* strerrbuf, size_t buflen) {
  __require_allocated_array(strerrbuf);
  INFER_EXCLUDE_CONDITION(__get_array_length(strerrbuf) < buflen);

  return __infer_nondet_int();
}
#endif

ssize_t write(int fildes, const void* buf, size_t nbyte) {
  __require_allocated_array(buf);
  INFER_EXCLUDE_CONDITION(__get_array_length(buf) < nbyte);

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || nbyte < ret);
  return ret;
}

int creat(const char* path, mode_t mode) {
  __require_allocated_array(path);

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1);
  return ret;
}

// modeled as skip
int fcntl(int fildes, int cmd, ...) {
  int ret = __infer_nondet_int();
  return ret;
}

// modelled as skip
int sigprocmask(int how, const sigset_t* set, sigset_t* oset) {
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || 0 < ret);
  return ret;
}

// modelled as skip
void setbuf(FILE* __restrict stream, char* __restrict buf) {}

// return nondeterministically 0 or EOF
int setvbuf(FILE* __restrict stream,
            char* __restrict buf,
            int mode,
            size_t size) {
  int n;

  n = __infer_nondet_int();
  if (n)
    return 0;
  else
    return EOF;
}

// the array base must be allocated with at least nmemb elements
// nondeterministically return 0 or a pointer inside the array
void* bsearch(const void* key,
              const void* base,
              size_t nmemb,
              size_t size,
              int (*compar)(const void*, const void*)) {
  int base_size;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();
  __require_allocated_array(base);
  base_size = __get_array_length(base);
  INFER_EXCLUDE_CONDITION(nmemb > base_size);
  if (nondet)
    return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= nmemb);
  return (char*)base + offset;
}

// return a nondeterministic value
int mblen(const char* s, size_t n) { return __infer_nondet_int(); }

// return a nondeterministic value
size_t mbstowcs(wchar_t* __restrict pwcs, const char* __restrict s, size_t n) {
  return (size_t)__infer_nondet_int();
}

// return a nondeterministic value
int mbtowc(wchar_t* __restrict pwc, const char* __restrict s, size_t n) {
  return __infer_nondet_int();
}

// modeled as skip
void qsort(void* base,
           size_t nmemb,
           size_t size,
           int (*compar)(const void*, const void*)) {}

// return a nondeterministic value
int strcoll(const char* s1, const char* s2) { return __infer_nondet_int(); }

// return a nondeterministic value
size_t wcstombs(char* __restrict s, const wchar_t* __restrict pwcs, size_t n) {
  return (size_t)__infer_nondet_int();
}

// return a nondeterministic value
int wctomb(char* s, wchar_t wc) { return __infer_nondet_int(); }

// modeled like open
int socket(int namespace_, int style, int protocol) {
  int* ret = (int*)malloc(sizeof(int));
  if (ret) {
    __set_file_attribute(ret);
    INFER_EXCLUDE_CONDITION(ret < (int*)1); // force result to be > 0
    return (size_t)ret;
  }
  return -1;
}

void* xcalloc(size_t nmemb, size_t size) {
  void* ret = calloc(nmemb, size);
  INFER_EXCLUDE_CONDITION(ret == NULL);
  return ret;
}

void* xmalloc(size_t size) {
  void* ret = malloc(size);
  INFER_EXCLUDE_CONDITION(ret == NULL);
  return ret;
}
