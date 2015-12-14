/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <locale.h>
#include <dirent.h>
#include <pwd.h>
#include <stdarg.h>
#include <limits.h>

#include <pthread.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/shm.h>
#include <sys/stat.h>
#ifdef __APPLE__ // includes for statfs are system-dependent
#include <sys/param.h>
#include <sys/mount.h>
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

#ifdef __CYGWIN__ // define __WAIT_STATUS as int * on cygwin
#define __WAIT_STATUS    int *
#endif
#ifdef __APPLE__ // define __WAIT_STATUS as int * on mac
#define __WAIT_STATUS    int *
#endif

//long __builtin_expect(long, long);
void abort(void); // builtin: modeled internally
int access(const char *path, int mode);
char *asctime(const struct tm *timeptr);
double atof(const char *str);
int atoi(const char *str);
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
void *calloc(size_t nmemb, size_t size);
#ifdef clearerr // cygwin defines clearerr as a macro
#undef clearerr
#endif
void clearerr(FILE *stream);
clock_t clock();
int close(int fildes);
int closedir(DIR *dirp);
size_t confstr(int name, char *buf, size_t len);
int creat(const char *path, mode_t mode);
char *ctime(const time_t *clock);
double difftime(time_t time1, time_t time0);
int dup(int fildes);
void endpwent(void);
void exit(int status); // builtin: modeled internally
int fclose (FILE *stream);
int fcntl(int fildes, int cmd, ...);
FILE *fdopen(int fildes, const char *mode);
#ifdef feof // cygwin defines feof as a macro
#undef feof
#endif
int feof(FILE *stream);
#ifdef ferror // cygwin defines ferror as a macro
#undef ferror
#endif
int ferror(FILE *stream);
int fflush(FILE *stream);
int fgetc(FILE *stream);
int fgetpos(FILE *__restrict stream, fpos_t *__restrict pos);
char *fgets( char *str, int num, FILE *stream );
int fileno(FILE *stream);
int flock(int fd, int operation);
FILE *fopen (const char *filename, const char *mode);
pid_t fork(void);
int fprintf(FILE *stream, const char *format, ...);
int fputc(int c, FILE *stream);
int fputs(const char *str, FILE *stream);
size_t fread(void *__restrict ptr, size_t size, size_t nmemb, FILE *__restrict stream);
void free(void *ptr); // builtin: modeled internally
FILE *freopen(const char *__restrict filename, const char *__restrict mode, FILE *__restrict stream);
int fscanf(FILE *stream, const char *format, ...); // builtin: modeled internally
int fsctl(const char *path, unsigned long request, void *data, unsigned int options);
int fseek(FILE *stream, long int offset, int whence);
int fsetpos(FILE *stream, const fpos_t *pos);
int fstat(int fildes, struct stat *buf);
int fsync(int fildes);
long int ftell(FILE *stream);
int futimes(int fildes, const struct timeval times[2]);
size_t fwrite(const void *__restrict ptr, size_t size, size_t nmemb, FILE *__restrict stream);
int getc(FILE *stream);
#ifdef getchar // cygwin defines getchar as a macro
#undef getchar
#endif
int getchar(void);
char *getcwd (char *buffer, size_t size);
char *getenv(const char *name);
char *getlogin();
char *getpass(const char *prompt);
pid_t getpid(void);
struct passwd *getpwent(void);
struct passwd *getpwnam(const char *login);
struct passwd *getpwuid(uid_t uid);
int getrusage(int who, struct rusage *r_usage);
char *gets(char *s);
#ifdef __APPLE__
#define gettimeofday_tzp_decl void *__restrict tzp
#else
#ifdef __CYGWIN__
#define gettimeofday_tzp_decl void *__restrict tzp
#else
#define gettimeofday_tzp_decl struct timezone *__restrict tzp
#endif
#endif
int gettimeofday(struct timeval *__restrict tp, gettimeofday_tzp_decl);
uid_t getuid(void);
struct tm *gmtime(const time_t *timer);
int isalnum(int x);
int isalpha(int x);
int isascii(int x);
int isatty(int fildes);
int isblank(int x);
int iscntrl(int x);
int isdigit(int x);
int isgraph(int x);
int islower(int x);
int isprint(int x);
int ispunct(int x);
int isspace(int x);
int isupper(int x);
int isxdigit(int x);
struct lconv *localeconv();
struct tm *localtime(const time_t *clock);
struct tm *localtime_r(const time_t *__restrict timer, struct tm *__restrict result);
void longjmp(jmp_buf env, int val);
off_t lseek(int fildes, off_t offset, int whence);
void *malloc(size_t size); // builtin: modeled internally
int mblen(const char *s, size_t n);
size_t mbstowcs(wchar_t * __restrict pwcs, const char * __restrict s, size_t n);
int mbtowc(wchar_t * __restrict pwc, const char * __restrict s, size_t n);
void *memchr(const void *s, int c, size_t n);
int memcmp(const void *s1, const void *s2, size_t n);
void *memcpy(void *s1, const void *s2, size_t n);
void *memmove(void *s1, const void *s2, size_t n);
void *memset(void *s, int c, size_t n);
int mkdir (const char *filename, mode_t mode);
time_t mktime(struct tm *timeptr);
int munmap(void *addr, size_t len);
int open(const char *path, int oflag, ...);
DIR *opendir(const char *dirname);
int pause(void);
void perror(const char *s);
int pipe(int fildes[2]);
int printf(const char *format, ...);
int pthread_create(pthread_t *thread, const pthread_attr_t *attr, void *(*start_routine)(void *), void *arg); // builtin: modeled internally
void pthread_exit(void *value_ptr);
int pthread_mutexattr_destroy(pthread_mutexattr_t *attr);
int pthread_mutexattr_init(pthread_mutexattr_t *attr);
int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type);
int pthread_mutexattr_gettype(const pthread_mutexattr_t *attr, int *type);
int pthread_mutex_destroy(pthread_mutex_t *mutex);
int pthread_mutex_init(pthread_mutex_t *__restrict mutex, const pthread_mutexattr_t *__restrict attr);
int pthread_mutex_lock(pthread_mutex_t *mutex);
int pthread_mutex_trylock(pthread_mutex_t *mutex);
int pthread_mutex_unlock(pthread_mutex_t *mutex);
int putc(int c, FILE *stream);
int puts(const char *str);
void qsort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
int raise(int sig);
long random(void);
struct dirent *readdir(DIR *dirp);
ssize_t read(int fildes, void *buf, size_t nbyte);
char *readline(const char *prompt);
void *realloc(void *ptr, size_t size);
int remove(const char *path);
int rename(const char *old, const char *new);
void rewind(FILE *stream);
int scanf(const char *format, ...); // builtin: modeled internally
void setbuf(FILE * __restrict stream, char * __restrict buf);
int setitimer(int which, const struct itimerval *__restrict value, struct itimerval *__restrict ovalue);
char *setlocale(int category, const char *locale);
int setlogin(const char *name);
int setpassent(int stayopen);
void setpwent(void);
int setvbuf(FILE * __restrict stream, char * __restrict buf, int mode, size_t size);
void *shmat(int shmid, const void *shmaddr, int shmflg);
int shmctl(int shmid, int cmd, struct shmid_ds *buf);
int shmdt(const void *shmaddr);
int shmget(key_t key, size_t size, int shmflg);
int sigaction(int sig, const struct sigaction *act, struct sigaction *oact);
void (*signal(int sig, void (*func)(int)))(int);
int sigprocmask(int how, const sigset_t *set, sigset_t *oset);
unsigned sleep(unsigned seconds);
int snprintf(char * __restrict s, size_t n, const char * __restrict format, ...);
int socket (int namespace, int style, int protocol);
int sprintf(char *s, const char *format, ...);
int sscanf(const char *s, const char *format, ...); // builtin: modeled internally
int stat(const char *path, struct stat *buf);
int statfs(const char *path, struct statfs *buf);
char *strcat(char *s1, const char *s2);
char *strchr(const char *s, int c);
int strcmp(const char *s1, const char *s2);
int strcoll(const char *s1, const char *s2);
char *strcpy(char *s1, const char *s2);
size_t strcspn(const char *s1, const char *s2);
char *strdup(const char *s);
size_t strftime(char *__restrict s, size_t maxsize, const char *__restrict format, const struct tm *__restrict timeptr);
int strerror_r(int errnum, char *strerrbuf, size_t buflen);
size_t strlcat(char *dst, const char *src, size_t size);
size_t strlcpy(char *dst, const char *src, size_t size);
size_t strlen(const char *s);
char *strlwr(char *s);
char *strncat(char *s1, const char *s2, size_t n);
int strncmp(const char *s1, const char *s2, size_t n);
char *strncpy(char *s1, const char *s2, size_t n);
char *strpbrk(const char *s1, const char *s2);
char *strrchr(const char *s, int c);
size_t strspn(const char *s1, const char *s2);
char *strstr(const char *s1, const char *s2);
double strtod(const char *str, char **endptr);
long strtol(const char *str, char **endptr, int base);
unsigned long strtoul(const char *str, char **endptr, int base);
char *strupr(char *s);
time_t time(time_t *tloc);
FILE *tmpfile(void);
char *tmpnam(char *s);
int toascii(int x);
int tolower(int x);
int toupper(int x);
int ungetc(int c, FILE *stream);
int unlink (const char *filename);
int usleep(useconds_t useconds);
int utimes(const char *path, const struct timeval times[2]);
int vfprintf(FILE *stream, const char *format, va_list arg);
int vfscanf(FILE *stream, const char *format, va_list arg); // builtin: modeled internally
int vprintf(const char *format, va_list arg);
int vscanf(const char *format, va_list arg); // builtin: modeled internally
int vsnprintf(char *s, size_t n, const char *format, va_list arg);
int vsprintf(char *s, const char *format, va_list arg);
int vsscanf(const char *s, const char *format, va_list arg); // builtin: modeled internally
pid_t wait(__WAIT_STATUS stat_loc);
size_t wcstombs(char * __restrict s, const wchar_t * __restrict pwcs, size_t n);
int wctomb(char *s, wchar_t wc);
ssize_t write(int fildes, const void *buf, size_t nbyte);
void *xcalloc(size_t nmemb, size_t size);
void *xmalloc(size_t size);

// modelling of errno
// errno expands to different function calls on mac or other systems
// the function call returns the address of a global variable called "errno"
extern int errno;
#ifdef __APPLE__
#define __ERRNO_FUN_NAME __error
#else
#define __ERRNO_FUN_NAME __errno_location
#endif
int *__ERRNO_FUN_NAME() {
  return &errno;
}

// the strings s1 and s2 need to be allocated
// check that s2 fits inside s1
char *strcpy(char *s1, const char *s2) {
  int size1;
  int size2;
  __infer_set_flag("ignore_return", ""); // no warnings if the return value is ignored

  size1 = __get_array_size(s1);
  size2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION(size2>size1);
  return s1;
}

// the string s must be allocated; return the result of malloc with the same size
char *strdup(const char *s) {
  int size;
  size = __get_array_size(s);
  return malloc(size);
}

// the strings s1 and s2 need to be allocated
// check that s2 fits inside s1
char *strcat(char *s1, const char *s2) {
  int size1;
  int size2;

  size1 = __get_array_size(s1);
  size2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION(size2>size1);
  return s1;
}

// the string s must be allocated
// nondeterministically return 0 or a pointer inside the buffer
char *strchr(const char *s, int c) {
  int size;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();

  size = __get_array_size(s);
  if(nondet) return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >=size);
  return (void *)s + offset;
}

// s1 and s2 must be allocated.
// return a non-deterministic integer
int strcmp(const char *s1, const char *s2) {
  int size_s1;
  int size_s2;
  int res;
  res = __infer_nondet_int();

  size_s1 = __get_array_size(s1);
  size_s2 = __get_array_size(s2);
  return res;
}

// the string s must be allocated
// return the size of the buffer - 1
size_t strlen(const char *s) {
  int size;
  size = __get_array_size(s);
  return size-1;
}

// s must be allocated
// return s
char *strlwr(char *s) {
  int size1;
  size1 = __get_array_size(s);
  return s;
}

// s1 and s2 must be allocated
// n should not be greater than the size of s1 or s2
// return s1
char *strncat(char *s1, const char *s2, size_t n) {
  int size_s1;
  int size_s2;

  size_s1 = __get_array_size(s1);
  size_s2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION((n>size_s1) || (n>size_s2));
  return s1;
}

// s1 and s2 must be allocated
// n should not be greater than the size of s1 or s2
// return a non-deterministic integer
int strncmp(const char *s1, const char *s2, size_t n) {
  int size_s1;
  int size_s2;
  int res;
  res = __infer_nondet_int();

  size_s1 = __get_array_size(s1);
  size_s2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION((n>size_s1) || (n>size_s2));
  return res;
}

// the strings s1 and s2 need to be allocated
// check that n characters fit in s1 (because even if s2 is shorter than n, null characters are appended to s1)
char *strncpy(char *s1, const char *s2, size_t n) {
  int size1, size2;
  __infer_set_flag("ignore_return", ""); // no warnings if the return value is ignored

  size1 = __get_array_size(s1);
  size2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION(n > size1);
  return s1;
}

// the strings s1 and s2 must be allocated
// nondeterministically return 0 or a pointer inside the string s1
char *strpbrk(const char *s1, const char *s2) {
  int size1, size2;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();

  size1 = __get_array_size(s1);
  size2 = __get_array_size(s2);
  if(nondet) return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >=size1);
  return (void *)s1 + offset;
}

// modelled like strchr
char *strrchr(const char *s, int c) {
  return strchr(s,c);
}

// s1 and s2 must be allocated.
// return an integer between 0 ans the size of s1
size_t strspn(const char *s1, const char *s2) {
  int size_s1;
  int size_s2;
  int res;
  res = __infer_nondet_int();

  size_s1 = __get_array_size(s1);
  size_s2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION(res < 0 || res > size_s1);
  return res;
}

// the strings s1 and s2 must be allocated
// nondeterministically return 0 or a pointer inside the string s1
char *strstr(const char *s1, const char *s2) {
  int size1, size2;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();

  size1 = __get_array_size(s1);
  size2 = __get_array_size(s2);
  if(nondet) return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >=size1);
  return (void *)s1 + offset;
}

// modeled using strtoul
double strtod(const char *str, char **endptr) {
  return (double) strtoul(str, endptr, 0);
}

// modeled like strtoul
long strtol(const char *str, char **endptr, int base) {
  return (long) strtoul(str, endptr, base);
}

// the string s must be allocated
// assign to endptr a pointer somewhere inside the string str
// result is nondeterministic
unsigned long strtoul(const char *str, char **endptr, int base) {
  int size;
  int offset;
  int res;
  size = __get_array_size(str);
  offset = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= size);
  if(endptr) *endptr = (char *) (str + offset);
  res = __infer_nondet_int();
  int errno_val = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(errno_val < 0);
  errno = errno_val;
  return res;
}

// s must be allocated
// return s
char *strupr(char *s) {
  int size1;
  size1 = __get_array_size(s);
  return s;
}

// the array s must be allocated
// n should not be greater than the size of s
// nondeterministically return 0 or a pointer within the first n elements of s
void *memchr(const void *s, int c, size_t n) {
  int size;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();

  size = __get_array_size(s);
  INFER_EXCLUDE_CONDITION(n > size);
  if(nondet) return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= n);
  return (void *)s + offset;
}

// s1 and s2 must be allocated
// n should not be greater than the size of s1 or s2
// return a non-deterministic integer
int memcmp(const void *s1, const void *s2, size_t n) {
  int size_s1;
  int size_s2;

  size_s1 = __get_array_size(s1);
  size_s2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION((n>size_s1) || (n>size_s2));
  return __infer_nondet_int();
}

// s1 and s2 must be allocated
// n must be between 0 and the minumum of the sizes of s1 and s2
void *memcpy(void *s1, const void *s2, size_t n) {
  int size_s1;
  int size_s2;
  __infer_set_flag("ignore_return", ""); // no warnings if the return value is ignored

  size_s1 = __get_array_size(s1);
  size_s2 = __get_array_size(s2);
  INFER_EXCLUDE_CONDITION((n < 0) || (n>size_s1) || (n>size_s2));
  return s1;
}

// modeld using memcpy
void *memmove(void *s1, const void *s2, size_t n) {
  __infer_set_flag("ignore_return", ""); // no warnings if the return value is ignored
  return memcpy(s1, s2, n);
}

// s needs to be allocated
// n should not be greater than the size of s
void *memset(void *s, int c, size_t n) {
  int size_s;
  __infer_set_flag("ignore_return", ""); // no warnings if the return value is ignored

  size_s = __get_array_size(s);
  INFER_EXCLUDE_CONDITION(n>size_s);
  return s;
}

// return a nondeterministic double
double atof(const char *str) {
  return __infer_nondet_double();
}

// return a nondeterministic value between INT_MIN and INT_MAX
int atoi(const char *str) {
  int retu = INT_MAX;
  int retl = INT_MIN;
  int ret;
  if (__infer_nondet_int())  ret = retu;
  else ret = retl;
  return ret;
}

// modeled using malloc and set file attribute
FILE *fopen (const char *filename, const char *mode) {
  FILE *ret;
  ret = malloc(sizeof(FILE));
  if(ret) __set_file_attribute(ret);
  return ret;
}

// modeled using fopen
FILE *tmpfile(void) {
  return fopen("foo", "");
}

// use a global variable to model the return value of tmpnam
extern char *_tmpnam_global;
// return NULL, or if s is NULL return a global variable, otherwise check that s has size at least L_tmpnam and return s
char *tmpnam(char *s) {
  int success;
  int size;

  success = __infer_nondet_int();
  if(!success) return NULL;
  if(s) {
    size = __get_array_size(s);
    INFER_EXCLUDE_CONDITION(size < L_tmpnam);
    return s;
  }
  else return _tmpnam_global;
}

// nondeterministically return NULL or the original stream
FILE *freopen(const char *__restrict filename, const char *__restrict mode, FILE *__restrict stream) {
  int n;
  n = __infer_nondet_int();
  if(n) return NULL;
  else return stream;
}

// modeled using free, can return EOF
int fclose (FILE *stream) {
  int n;
  free(stream);
  n = __infer_nondet_int();
  if (n>0) return 0;
  else return EOF;
}

// modeled using malloc and set file attribute
// return the allocated ptr - 1 if malloc succeeds, otherwise return -1
int open(const char *path, int oflag, ...) {
  int *ret = malloc(sizeof(int));
  if(ret) {
    __set_file_attribute(ret);
    INFER_EXCLUDE_CONDITION(ret < (int *)1); // force result to be > 0
    return ret;
  }
  return -1;
}

// modeled using free, can return -1
int close(int fildes) {
  int n;
  if (fildes != -1)
    free((int *) (long) fildes);
  n = __infer_nondet_int();
  if (n>0) return 0;
  else return -1;
}

// modeled as close followed by fopen
FILE *fdopen(int fildes, const char *mode) {
  close(fildes);
  return fopen("foo", mode);
}

// return nonteterministically 0 or -1
// requires stream to be allocated
int fseek(FILE *stream, long int offset, int whence) {
  int n;
  FILE tmp;
  tmp = *stream;
  n = __infer_nondet_int();
  if (n) return 0;
  else return -1;
}

// return nondeterministically a nonnegative value or -1
// requires stream to be allocated
long int ftell(FILE *stream) {
  int n;
  FILE tmp;
  tmp = *stream;
  n = __infer_nondet_int();
  if (n>=0) return n;
  else return -1;
}

// on success return str otherwise null
// requires stream to be allocated
char *fgets( char *str, int num, FILE *stream ) {
  int n;
  int size1;
  FILE tmp;

  tmp = *stream;
  n = __infer_nondet_int();

  if (n>0) {
    size1 = __get_array_size(str);
    INFER_EXCLUDE_CONDITION(num>size1);
    return str;
  }
  else return NULL;
}

// string s must be allocated; return nondeterministically s or NULL
char *gets(char *s) {
  int n;
  int size;

  size = __get_array_size(s);
  n = __infer_nondet_int();
  if(n) return s;
  else return NULL;
}

// str must be allocated, return a nondeterministic value
int puts(const char *str) {
  int size1;
  size1 = __get_array_size(str);
  return __infer_nondet_int();
}

// modeled using puts
// require the stream to be allocated
int fputs(const char *str, FILE *stream) {
  FILE tmp;
  tmp = *stream;
  return puts(str);
}

// return a nondeterministic value
// requires stream to be allocated
int getc(FILE *stream)
{
  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

// return a nondeterministic value
// requires stream to be allocated
int fgetc(FILE *stream)
{
  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

// return nondeterministically c or EOF
// requires stream to be allocated
int ungetc(int c, FILE *stream) {
  int n;
  FILE tmp;
  tmp = *stream;

  n = __infer_nondet_int();
  if (n) return c;
  else return EOF;
}

// modeled like putc
// requires stream to be allocated
int fputc(int c, FILE *stream)
{
  FILE tmp;
  tmp = *stream;
  return putc(c,stream);
}

// on success return buffer otherwise null
char *getcwd (char *buffer, size_t size) {
  int n;
  int size_buf;
  n = __infer_nondet_int();

  if (n>0) {
    size_buf= __get_array_size(buffer);
    INFER_EXCLUDE_CONDITION(size>size_buf);
    return buffer;
  }
  else return NULL;
}

// return nonteterministically 0 or -1
int rename(const char *old, const char *new) {
  int n;

  n = __infer_nondet_int();
  if (n) return 0;
  else return -1;
}

// modeled as skip
// requires stream to be allocated
void rewind(FILE *stream) {
    FILE tmp;
    tmp = *stream;
}

// modeled as exit()
void longjmp(jmp_buf env, int val) {
  exit(0);
}

// modeled as skip.
//If sleep() returns because the requested time has elapsed,
//the value returned shall be 0. If sleep() returns due to delivery of a signal,
//the return value shall be the "unslept" amount (the requested time minus the time actually slept) in seconds.
unsigned sleep(unsigned seconds) {
  int n;
  n = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(n<0);
  return n;
}

// the waiting is modeled as skip. Then the return value is a random value of a process or
// -1 in case of an error
pid_t wait(__WAIT_STATUS stat_loc) {
  int n;
  pid_t tmp;
  n = __infer_nondet_int();
  tmp = __infer_nondet_int();
  if (n>0) return tmp;
  else return -1;
}

// return a nondeterministic pointer
void (*signal(int sig, void (*func)(int)))(int) {
  void (*res) (int) = __infer_nondet_ptr();
  return res;
};

// modelled as exit
void pthread_exit(void *value_ptr) { exit(0); };

//INTENDED SEMANTICS: The setitimer() function shall set the timer specified by which
//to the value specified in the structure pointed to by value, and if ovalue is not a null pointer,
//store the previous value of the timer in the structure pointed to by ovalue.
int setitimer(int which, const struct itimerval *__restrict value, struct itimerval *__restrict ovalue) {
  int n;
  int tmp;
  n = __infer_nondet_int();
  tmp = __infer_nondet_int();
  // not sure about this assignment.
  //it should somehow change the value of the timer which.
  //But which is just an int, so it looks like this is useless
  which=tmp;
  if (n>0) return 0;
  else return -1;
}

// pause returns only when a signal is received
// the return value is always -1
int pause(void) {
  return -1;
}

// modeled with nondeterministic value n
pid_t fork(void) {
  return __infer_nondet_int();
}

// allocate memory directly, and return -1 if malloc fails.
int shmget(key_t key, size_t size, int shmflg) {
  void *res = malloc(size);
  if(!res) return -1;
  return (long) res;
}

// simply return the first parameter
void *shmat(int shmid, const void *shmaddr, int shmflg) {
  return (void *) (long) shmid;
}

// return a non-deterministic value
int shmdt(const void *shmaddr) {
  return __infer_nondet_int();
}

// if the command is IPC_RMID free the first parameter; return a non-deterministic value
int shmctl(int shmid, int cmd, struct shmid_ds *buf) {
  int n;
  void *shmaddr;
  n = __infer_nondet_int();
  shmaddr = (void *) (long) shmid;
  if(cmd == IPC_RMID) free(shmaddr);
  return n;
}

void *realloc(void *ptr, size_t size) {
  if(ptr==0) { // if ptr in NULL, behave as malloc
    return malloc(size);
  }
  int old_size;
  int can_enlarge;
  old_size = __get_array_size(ptr); // force ptr to be an array
  can_enlarge = __infer_nondet_int(); // nondeterministically choose whether the current block can be enlarged
  if(can_enlarge) {
    __set_array_size(ptr, size); // enlarge the block
    return ptr;
  }
  int *newblock = malloc(size);
  if(newblock) {
    free(ptr);
    return newblock;
  }
  else { // if new allocation fails, do not free the old block
    return newblock;
  }
}

// modelled as a call to malloc
void *calloc(size_t nmemb, size_t size) {
  return malloc(nmemb *size);
}

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

//modeled as skip. n>0 models success.
//Upon successful completion a non-negative integer, namely the file descriptor, shall be returned;
// otherwise, -1 shall be returned.
int dup(int fildes) {
  int n;
  n = __infer_nondet_int();
  if (n>0) return fildes;
  else return -1;

};

// modeled as skip.
//The getuid() function shall always be successful and no return value is reserved to indicate the error.
uid_t getuid(void) {
  return __infer_nondet_int();
};


//modeled as skip
//The getpid() function shall always be successful and no return value is reserved to indicate an error.
pid_t getpid(void) {
  return __infer_nondet_int();
};

// modeled as skip.
// success is modeled by n>0, if lseek fails (off_t)-1 should be returned.
off_t lseek(int fildes, off_t offset, int whence) {
  int n;
  off_t tmp;
  n = __infer_nondet_int();
  tmp = __infer_nondet_int();

  if (n>0) return tmp;
  else return (off_t)-1;
};

// modeled using malloc and set file attribute
DIR *opendir(const char *dirname) {
  DIR *ret;
  ret = malloc(sizeof(void *));
  if(ret) __set_file_attribute(ret);
  return ret;
}

// modeled using free, can return EOF
int closedir(DIR *dirp) {
  DIR tmp = *dirp;
  int n;
  n = __infer_nondet_int();
  free(dirp);
  if (n>0) return 0;
  else return EOF;
}

// use a global variable to model the return value of readdir
extern struct dirent *_dirent_global;
// dirp must be allocated
// return 0 or the allocated pointerq _dirent_global, nondeterministically
struct dirent *readdir(DIR *dirp) {
  int len;
  int nondet;
  struct dirent *ret = _dirent_global;
  len = __get_array_size(dirp);
  nondet = __infer_nondet_int();
  if(nondet) return 0;
  return ret;
}

// use a global variable to model the return value of getenv
extern char *_getenv_global;
// string name must be allocated
// return 0 or the allocated string _getenv_global, nondeterministically
char *getenv(const char *name) {
  int size;
  int nondet;
  size =__get_array_size(name);
  size =__get_array_size(_getenv_global);
  nondet = __infer_nondet_int();
  if(nondet) return 0;
  return _getenv_global;
}

// use a global variable to model the return value of setlocale
extern char *_locale_global;
// string locale must be allocated
// return 0 or the allocated string _locale_global, nondeterministically
char *setlocale(int category, const char *locale) {
  int size;
  int nondet;
  size =__get_array_size(locale);
  size =__get_array_size(_locale_global);
  nondet = __infer_nondet_int();
  if(nondet) return 0;
  return _locale_global;
}

// use a global variable to model the return value of localeconv
extern struct lconv *_lconv_global;
// return the allocated pointer _lconv_global
struct lconv *localeconv() {
  int nondet;
  struct lconv tmp;
  tmp = *_lconv_global;
  return _lconv_global;
}

// use a global variable to model the return value of localtime
extern struct tm *_tm_global;
// return nondeterministically NULL or the global variable _tm_global
struct tm *localtime(const time_t *clock) {
  int fail;

  fail = __infer_nondet_int();
  if (fail) return NULL;
  else return _tm_global;
}

// return nondeterministically NULL or the global variable _tm_global
struct tm *gmtime(const time_t *timer)
{
  int fail;

  fail = __infer_nondet_int();
  if (fail) return NULL;
  else return _tm_global;
}

// use a global variable to model the return value of asctime
extern char *_asctime_global;
// return the global variable _asctime_global
char *asctime(const struct tm *timeptr)
{
  return _asctime_global;
}

// modelled using asctime and localtime
char *ctime(const time_t *clock) {
  return asctime(localtime(clock));
}

// return a nondeterministic double
double difftime(time_t time1, time_t time0)
{
  return __infer_nondet_double();
}

// return a nondeterministic nonnegative value or -1
time_t mktime(struct tm *timeptr)
{
  time_t res;
  res = __infer_nondet_time_t();
  INFER_EXCLUDE_CONDITION(res < -1);
  return res;
}

// return a nondeterministic nonnegative value or -1
clock_t clock()
{
  clock_t res;
  res = __infer_nondet_clock_t();
  INFER_EXCLUDE_CONDITION(res < -1);
  return res;
}

// return a nondeterministic nonnegative value
size_t strftime(char *__restrict s, size_t maxsize, const char *__restrict format, const struct tm *__restrict timeptr)
{
  size_t res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic value of type time_t
time_t time(time_t *tloc) {
  time_t t;
  t = __infer_nondet_time_t();
  if(tloc) *tloc = t;
  return t;
}

// use a global variable to model the return value of getpwuid
extern struct passwd *_getpwuid_global;
// return either NULL or the global variable _getpwuid_global
struct passwd *getpwuid(uid_t uid) {
  int found;
  found = __infer_nondet_int();
  if (found) return _getpwuid_global;
  else return NULL;
}

// use a global variable to model the return value of getpwent
extern struct passwd *_getpwent_global;
// return either NULL or the global variable _getpwent_global
struct passwd *getpwent(void) {
  int found;
  found = __infer_nondet_int();
  if (found) return _getpwent_global;
  else return NULL;
}

// use a global variable to model the return value of getpwnam
extern struct passwd *_getpwnam_global;
// login must be allocated
// return either NULL or the global variable _getpnam_global
struct passwd *getpwnam(const char *login) {
  int found;
  int size1;
  size1 = __get_array_size(login);
  found = __infer_nondet_int();
  if (found) return _getpwnam_global;
  else return NULL;
}

// nondeterministically return 0 (failure) or 1 (success)
int setpassent(int stayopen) {
  int success;
  success = __infer_nondet_int();
  if(success) return 1;
  else return 0;
}

// modled as skip
void setpwent(void) {
}

// modled as skip
void endpwent(void) {
}

// use a global variable to model the return value of getlogin
extern char *_getlogin_global;
// string name must be allocated
// return 0 or the allocated string _getlogin_global, nondeterministically
char *getlogin() {
  int size;
  int nondet;
  size = __get_array_size(_getlogin_global);
  nondet = __infer_nondet_int();
  if(nondet) return 0;
  return _getlogin_global;
}

int setlogin(const char *name) {
  int success;
  int size1;
  size1 = __get_array_size(name);
  success = __infer_nondet_int();
  if (success) {
    strcpy(_getlogin_global, name);
    return 0;
  }
  else return -1;
}

// use a global variable to model the return value of getpass
extern char *_getpass_global;
// prompt must be allocated
// return the global variable _getpass_global, which must be allocated
char *getpass(const char *prompt) {
  int size1;
  size1 = __get_array_size(prompt);
  size1 = __get_array_size(_getpass_global);
  return _getpass_global;
}

// return a nondeterministic nonnegative integer
int printf(const char *format, ...)
{
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
// requires stream to be allocated
int fprintf(FILE *stream, const char *format, ...)
{
  int res;
  FILE tmp;
  tmp= *stream;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int snprintf(char * __restrict s, size_t n, const char * __restrict format, ...)
{
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// s must be allocated
// return a nondeterministic nonnegative integer
int sprintf(char *s, const char *format, ...)
{
  int res;
  int size1;
  size1 = __get_array_size(s);
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
// requires stream to be allocated
int vfprintf(FILE *stream, const char *format, va_list arg)
{
  int res;
  FILE tmp;

  tmp= *stream;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int vsprintf(char *s, const char *format, va_list arg)
{
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int vsnprintf(char *s, size_t n, const char *format, va_list arg)
{
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int vprintf(const char *format, va_list arg)
{
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// forces path to be allocated
//return nondeterministically 0 or -1
int utimes(const char *path, const struct timeval times[2]) {
  int size1;
  int success = __infer_nondet_int();

  size1= __get_array_size(path);
  //return random result
  return (success > 0) ? 0 : -1;
}


// forces filename  to be allocated
//return nondeterministically 0 or -1
int unlink (const char *filename) {
  int size1;
  int success = __infer_nondet_int();
  size1=__get_array_size(filename);
  return (success > 0) ? 0 : -1;
}


//return nondeterministically 0 or -1
int usleep(useconds_t useconds) {
  int success = __infer_nondet_int();
  return (success > 0) ? 0 : -1;
}

// dst and src must be allocated
// return an integer between 0 and size
size_t strlcpy(char *dst, const char *src, size_t size) {
  int size_src;
  int size_dst;
  int res;

  res = __infer_nondet_int();

  // force src to be allocated
  size_src = __get_array_size(src);

  // force dst to be allocated for at least size
  size_dst = __get_array_size(dst);
  INFER_EXCLUDE_CONDITION(size>size_dst);

  INFER_EXCLUDE_CONDITION(res > size || res < 0);

  return res;
}

// dst and src must be allocated
// return an integer between 0 and size
size_t strlcat(char *dst, const char *src, size_t size) {
  int size_src;
  int size_dst;
  int res;

  res = __infer_nondet_int();

  //force src to be allocated
  size_src = __get_array_size(src);

  //force dst to be allocated for at least size
  size_dst = __get_array_size(dst);
  INFER_EXCLUDE_CONDITION(size>size_dst);

  INFER_EXCLUDE_CONDITION(res > size || res < 0);

  return res;
}

// path must be allocated
// assign nonteterministically to the contents of buf
// return 0 or -1
int statfs(const char *path, struct statfs *buf) {
  int success;
  int size_path;

  success = __infer_nondet_int();

  // force path to be allocated
  size_path = __get_array_size(path);

  struct statfs s; // uninitialized struct statfs
  *buf = s;

  return (success > 0) ? 0 : -1;
}

// path must be allocated
// assign nonteterministically to the contents of buf
// return 0 or -1
int stat(const char *path, struct stat *buf) {
  int success;
  int size_path;

  success = __infer_nondet_int();

  // force path to be allocated
  size_path = __get_array_size(path);

  struct stat s; // uninitialized struct stat
  *buf = s;

  return (success > 0) ? 0 : -1;
}

int remove(const char *path) {
  int size_path;
  int success;

  //force path to be allocated
  size_path = __get_array_size(path);

  success = __infer_nondet_int();
  return (success > 0) ? 0 : -1;
}

char *readline(const char *prompt) {
  int size_prompt;
  char *ret;
  int size;

  //force prompt to be allocated when not null
  if (prompt != NULL){
    size_prompt = __get_array_size(prompt);
  }

  //return random string of positive size
  size = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(size<0);

  ret = malloc(sizeof(size));
  return ret;
}

long random(void) {
  long ret;

  ret = __infer_nondet_long_int();
  INFER_EXCLUDE_CONDITION(ret<0);
  return ret;
}

// requires stream to be allocated
int putc(int c, FILE *stream){
  int rand;
  FILE tmp;

  tmp = *stream;
  rand = __infer_nondet_int();
  if (rand > 0)
    return c; //success
  else
    return EOF; //failure
}

int access(const char *path, int mode){
  int size;
  int success;

  //force path to be allocated
  size = __get_array_size(path);

  //determine return value
  success = __infer_nondet_int();

  return (success > 0) ? 0 : -1;
}


size_t confstr(int name, char *buf, size_t len){
  int size;
  int ret;


  //determine return value
  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < 0);

  //buf should be allocated if len is not zero. Otherwise, we want buf=0.
  if (len) size = __get_array_size(buf);
  else INFER_EXCLUDE_CONDITION(buf !=0);

  return ret;
}

// return a non-deterministic value
// stream is not required to be allocated
int fflush(FILE *stream){
  return __infer_nondet_int();
}

int flock(int fd, int operation){
  int ret;

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

int fsync(int fildes){
  int ret;

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}


int fsctl(const char *path, unsigned long request, void *data, unsigned int options){
  int size1;
  int size2;
  int ret;

  //forces path and data to be allocated
  size1 = __get_array_size(path);
  size2 = __get_array_size(data);

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

int getrusage(int who, struct rusage *r_usage){
  int ret;

  INFER_EXCLUDE_CONDITION(r_usage == 0);

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}



int gettimeofday(struct timeval *__restrict tp, gettimeofday_tzp_decl){
  struct timeval tmp_tp;
  struct timezone tmp_tzp;
  int success;

  if (tp!=0) *tp = tmp_tp;
  if (tzp!=0) *(struct timezone *)tzp = tmp_tzp;

  success = __infer_nondet_int();
  return success ? 0 : -1;
}


struct tm *localtime_r(const time_t *__restrict timer, struct tm *__restrict result){
  int success;
  struct tm tmp;

  INFER_EXCLUDE_CONDITION(timer == 0);
  INFER_EXCLUDE_CONDITION(result == 0);

  success = __infer_nondet_int();
  *result = tmp;

  return (success > 0) ? result : 0;
}



int mkdir (const char *filename, mode_t mode){
  int size;
  int ret;

  size = __get_array_size(filename);

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}


int munmap(void *addr, size_t len){
  int ret;

  INFER_EXCLUDE_CONDITION(addr == 0);
  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);

  return ret;
}

// returns a nondeterministc value
int pthread_mutex_destroy(pthread_mutex_t *mutex){
  INFER_EXCLUDE_CONDITION(mutex == 0);
  return __infer_nondet_int();
}


// returns a nondeterministc value
int pthread_mutex_init(pthread_mutex_t *__restrict mutex, const pthread_mutexattr_t *__restrict attr){
  INFER_EXCLUDE_CONDITION(mutex == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutex_lock(pthread_mutex_t *mutex){
  INFER_EXCLUDE_CONDITION(mutex == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutex_trylock(pthread_mutex_t *mutex){
  INFER_EXCLUDE_CONDITION(mutex == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutex_unlock(pthread_mutex_t *mutex){
  INFER_EXCLUDE_CONDITION(mutex == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutexattr_destroy(pthread_mutexattr_t *attr){
  INFER_EXCLUDE_CONDITION(attr == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutexattr_init(pthread_mutexattr_t *attr){
  INFER_EXCLUDE_CONDITION(attr == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value
int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type){
  INFER_EXCLUDE_CONDITION(attr == 0);
  return __infer_nondet_int();
}

// returns a nondeterministc value and forces type to be allocated and set its content
int pthread_mutexattr_gettype(const pthread_mutexattr_t *attr, int *type){
  INFER_EXCLUDE_CONDITION(attr == 0);
  *type= __infer_nondet_int();
  return __infer_nondet_int();
}

// return a positive non-deterministic number or -1.
// requires stream to be allocated
int fileno(FILE *stream){
  int ret = __infer_nondet_int();
  FILE tmp;
  tmp = *stream;
  INFER_EXCLUDE_CONDITION(ret<-1 || ret==0 );
  return ret;
}

int fstat(int fildes, struct stat *buf) {
  int ret;
  struct stat s; // uninitialized struct stat
  *buf = s;
  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret>0);
  return ret;
}

// treates as skyp. Return nondeterministically 0 or -1
int futimes(int fildes, const struct timeval times[2]) {
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret>0);
  return ret;
}

int getchar(void) {
  //randomly produce an error
  int ret = __infer_nondet_int();
  if (ret < 0) return EOF;
  else return ret;
}

int isatty(int fildes) {
  int ret;

  ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < 0 || ret > 1);
  return ret;
}

void perror(const char *s) {
  int size = __get_array_size(s);
  size = 0;
}

int pipe(int fildes[2]) {
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > 0);
  return ret;
}

int raise(int sig){
  return __infer_nondet_int();
}

ssize_t read(int fildes, void *buf, size_t nbyte) {
  if (nbyte==0) return 0;
  INFER_EXCLUDE_CONDITION(__get_array_size(buf) < nbyte);

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret > nbyte);
  return ret;
}

int sigaction(int sig, const struct sigaction *act, struct sigaction *oact){
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || ret >0);
  return ret;
}

// return the first argument
//long __builtin_expect(long x, long y) {
//  return x;
//}
//TODO: this function has been disabled because it cannot be compiled with LLVM.
//It is normally a builtin that should not be implemented and LLVM complains about this

// modelled as skip
// stream is required to be allocated
void clearerr(FILE *stream) {
    FILE tmp;
    tmp = *stream;
}

// return a nondeterministic value
// stream required to be allocated
int ferror(FILE *stream) {

  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

// return a nondeterministic value
// stream required to be allocated
int feof(FILE *stream) {

  FILE tmp;
  tmp = *stream;
  return __infer_nondet_int();
}

// write to *pos and return either 0 or -1
// stream is required to be allocated
int fgetpos(FILE *__restrict stream, fpos_t *__restrict pos) {

  int success;
  FILE tmp;
  tmp = *stream;
  #ifdef __APPLE__  //fpos_t is a long in MacOS, but a struct in Linux.
    *pos = __infer_nondet_long();
  #else
    pos->__pos = __infer_nondet_long();
  #endif
  success = __infer_nondet_int();
  if(success) return 0;
  else return -1;
}

// read from *pos and return either 0 or -1
// stream is required to be allocated
int fsetpos(FILE *stream, const fpos_t *pos) {

  int success;
  FILE tmp;
  tmp = *stream;
  fpos_t t;
  t = *pos;
  success = __infer_nondet_int();
  if(success) return 0;
  else return -1;
}

// return a value between 0 and nmemb
size_t fread(void *__restrict ptr, size_t size, size_t nmemb, FILE *__restrict stream) {
  size_t res;
  res = __infer_nondet_size_t();
  if(size == 0 || nmemb == 0) return 0;
  INFER_EXCLUDE_CONDITION(res < 0 || res > nmemb);
  return res;
}

// return a value between 0 and nmemb
size_t fwrite(const void *__restrict ptr, size_t size, size_t nmemb, FILE *__restrict stream) {
  size_t res;
  res = __infer_nondet_size_t();
  if(size == 0 || nmemb == 0) return 0;
  INFER_EXCLUDE_CONDITION(res < 0 || res > nmemb);
  return res;
}

size_t strcspn(const char *s1, const char *s2){
  int size1, size2;
  size1 = __get_array_size(s1);
  size2 = __get_array_size(s2);;

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < 0);
  return ret;
}

int strerror_r(int errnum, char *strerrbuf, size_t buflen) {
  INFER_EXCLUDE_CONDITION(__get_array_size(strerrbuf) < buflen);

  return __infer_nondet_int();
}

ssize_t write(int fildes, const void *buf, size_t nbyte) {
  INFER_EXCLUDE_CONDITION(__get_array_size(buf) < nbyte);

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || nbyte < ret);
  return ret;
}

int creat(const char *path, mode_t mode) {
  int size;
  size = __get_array_size(path);

  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1);
  return ret;
}

// modeled as skip
int fcntl(int fildes, int cmd, ...){
  int ret = __infer_nondet_int();
  return ret;
}

// modelled as skip
int sigprocmask(int how, const sigset_t *set, sigset_t  *oset){
  int ret = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(ret < -1 || 0 < ret);
  return ret;
}

// modelled as skip
void setbuf(FILE * __restrict stream, char * __restrict buf) {
}

// return nondeterministically 0 or EOF
int setvbuf(FILE * __restrict stream, char * __restrict buf, int mode, size_t size) {
  int n;

  n = __infer_nondet_int();
  if (n) return 0;
  else return EOF;
}

// the array base must be allocated with at least nmemb elements
// nondeterministically return 0 or a pointer inside the array
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *))
{
  int base_size;
  int nondet;
  int offset;
  nondet = __infer_nondet_int();
  offset = __infer_nondet_int();

  base_size = __get_array_size(base);
  INFER_EXCLUDE_CONDITION(nmemb > base_size);
  if(nondet) return 0;
  INFER_EXCLUDE_CONDITION(offset < 0 || offset >= nmemb);
  return (void *)base + offset;
}

// return a nondeterministic value
int mblen(const char *s, size_t n) {
  return __infer_nondet_int();
}

// return a nondeterministic value
size_t mbstowcs(wchar_t * __restrict pwcs, const char * __restrict s, size_t n) {
  return (size_t)__infer_nondet_int();
}

// return a nondeterministic value
int mbtowc(wchar_t * __restrict pwc, const char * __restrict s, size_t n) {
  return __infer_nondet_int();
}

// modeled as skip
void qsort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *)) {
}

// return a nondeterministic value
int strcoll(const char *s1, const char *s2) {
  return __infer_nondet_int();
}

// return a nondeterministic value
size_t wcstombs(char * __restrict s, const wchar_t * __restrict pwcs, size_t n) {
  return (size_t)__infer_nondet_int();
}

// return a nondeterministic value
int wctomb(char *s, wchar_t wc) {
  return __infer_nondet_int();
}

// modeled like open
int socket(int namespace, int style, int protocol) {
  int *ret = malloc(sizeof(int));
  if(ret) {
    __set_file_attribute(ret);
    INFER_EXCLUDE_CONDITION(ret < (int *)1); // force result to be > 0
    return ret;
  }
  return -1;
}

void *xcalloc(size_t nmemb, size_t size) {
  void *ret = calloc(nmemb, size);
  INFER_EXCLUDE_CONDITION(ret == NULL);
  return ret;
}

void *xmalloc(size_t size) {
  void *ret = malloc(size);
  INFER_EXCLUDE_CONDITION(ret == NULL);
  return ret;
}
