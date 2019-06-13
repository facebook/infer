/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// modelling of wide character functions

#include "infer_builtins.h"

#define __asm(N)

// standard headers on mac don't use USE_CPP_OVERLOADS the way it's expected
#if defined __CORRECT_ISO_CPP_WCHAR_H_PROTO && !defined __APPLE__
#define USE_CPP_OVERLOADS
#endif

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>

#define restrict

#ifdef _WIN32
#define CLIBCALL __cdecl
#else
#define CLIBCALL
#endif

int CLIBCALL fwscanf(FILE* restrict stream,
                     const wchar_t* restrict format,
                     ...); // builtin: modeled internally
int CLIBCALL swscanf(const wchar_t* restrict s,
                     const wchar_t* restrict format,
                     ...); // builtin: modeled internally
int CLIBCALL vfwscanf(FILE* restrict stream,
                      const wchar_t* restrict format,
                      va_list arg); // builtin: modeled internally
int CLIBCALL vswscanf(const wchar_t* restrict s,
                      const wchar_t* restrict format,
                      va_list arg); // builtin: modeled internally
int CLIBCALL vwscanf(const wchar_t* restrict format,
                     va_list arg); // builtin: modeled internally
int CLIBCALL wscanf(const wchar_t* restrict format,
                    ...); // builtin: modeled internally

wint_t CLIBCALL btowc(int c) { return __infer_nondet_int(); }

wint_t CLIBCALL fgetwc(FILE* stream) { return __infer_nondet_int(); }

// modelled like fgets
wchar_t* CLIBCALL fgetws(wchar_t* restrict s, int n, FILE* restrict stream) {
  return (wchar_t*)fgets((char*)s, n, stream);
}

wint_t CLIBCALL fputwc(wchar_t c, FILE* stream) { return __infer_nondet_int(); }

// modeled using fputs
int CLIBCALL fputws(const wchar_t* restrict s, FILE* restrict stream) {
  return fputs((char*)s, stream);
}

int CLIBCALL fwide(FILE* stream, int mode) { return __infer_nondet_int(); }

// return a nondeterministic nonnegative integer
int CLIBCALL fwprintf(FILE* restrict stream,
                      const wchar_t* restrict format,
                      ...) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

#ifdef getwc
#undef getwc // disable expansion of getwc
#endif
wint_t CLIBCALL getwc(FILE* stream) { return __infer_nondet_int(); }

#ifdef getwchar
#undef getwchar // disable expansion of getwchar
#endif
wint_t CLIBCALL getwchar() { return __infer_nondet_int(); }

size_t CLIBCALL mbrlen(const char* restrict s,
                       size_t n,
                       mbstate_t* restrict ps) {
  return __infer_nondet_int();
}

size_t CLIBCALL mbrtowc(wchar_t* restrict pwc,
                        const char* restrict s,
                        size_t n,
                        mbstate_t* restrict ps) {
  return __infer_nondet_int();
}

int CLIBCALL mbsinit(const mbstate_t* ps) { return __infer_nondet_int(); }

size_t CLIBCALL mbsrtowcs(wchar_t* restrict dst,
                          const char** restrict src,
                          size_t len,
                          mbstate_t* restrict ps) {
  return __infer_nondet_int();
}

#ifdef putwc
#undef putwc // disable expansion of putwc
#endif
wint_t CLIBCALL putwc(wchar_t c, FILE* stream) { return __infer_nondet_int(); }

#ifdef putwchar
#undef putwchar // disable expansion of putwchar
#endif
wint_t CLIBCALL putwchar(wchar_t c) { return __infer_nondet_int(); }

// s must be allocated
// return a nondeterministic nonnegative integer
int CLIBCALL swprintf(wchar_t* restrict s,
                      size_t n,
                      const wchar_t* restrict format,
                      ...) {
  int res;
  int size1;
  size1 = __get_array_length(s);
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

wint_t CLIBCALL ungetwc(wint_t c, FILE* stream) { return __infer_nondet_int(); }

// return a nondeterministic nonnegative integer
int CLIBCALL vfwprintf(FILE* restrict stream,
                       const wchar_t* restrict format,
                       va_list arg) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int CLIBCALL vswprintf(wchar_t* restrict s,
                       size_t n,
                       const wchar_t* restrict format,
                       va_list arg) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// return a nondeterministic nonnegative integer
int CLIBCALL vwprintf(const wchar_t* restrict format, va_list arg) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

size_t CLIBCALL wcrtomb(char* restrict s, wchar_t wc, mbstate_t* restrict ps) {
  return __infer_nondet_int();
}

size_t CLIBCALL wcsrtombs(char* restrict dst,
                          const wchar_t** restrict src,
                          size_t len,
                          mbstate_t* restrict ps) {
  return __infer_nondet_int();
}

// return a nondeterministic nonnegative integer
int CLIBCALL wprintf(const wchar_t* restrict format, ...) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// modeled using strcat
wchar_t* CLIBCALL wcscat(wchar_t* restrict s1, const wchar_t* restrict s2) {
  return (wchar_t*)strcat((char*)s1, (char*)s2);
}

// modeled using strchr
#ifndef USE_CPP_OVERLOADS
wchar_t* CLIBCALL wcschr(const wchar_t* s, wchar_t c)
#else
const wchar_t* CLIBCALL wcschr(const wchar_t* s, wchar_t c) {
  return wcschr((wchar_t*)s, c);
}

wchar_t* CLIBCALL wcschr(wchar_t* s, wchar_t c)
#endif
{
  return (wchar_t*)strchr((char*)s, c);
}

// modeled using strcmp
int CLIBCALL wcscmp(const wchar_t* s1, const wchar_t* s2) {
  return strcmp((char*)s1, (char*)s2);
}

// modeled using strcmp
int CLIBCALL wcscoll(const wchar_t* s1, const wchar_t* s2) {
  return strcmp((char*)s1, (char*)s2);
}

// return a nondeterministic nonnegative integer
size_t CLIBCALL wcsftime(wchar_t* restrict s,
                         size_t maxsize,
                         const wchar_t* restrict format,
                         const struct tm* restrict timeptr) {
  int res;
  res = __infer_nondet_int();
  INFER_EXCLUDE_CONDITION(res < 0);
  return res;
}

// modeled using strcpy
wchar_t* CLIBCALL wcscpy(wchar_t* restrict s1, const wchar_t* restrict s2) {
  return (wchar_t*)strcpy((char*)s1, (char*)s2);
}

// modeled using strcmp
size_t CLIBCALL wcscspn(const wchar_t* s1, const wchar_t* s2) {
  return strcmp((char*)s1, (char*)s2);
}

// modeled using strlen
size_t CLIBCALL wcslen(const wchar_t* s) { return strlen((char*)s); }

// modeled using strncat
wchar_t* CLIBCALL wcsncat(wchar_t* restrict s1,
                          const wchar_t* restrict s2,
                          size_t n) {
  return (wchar_t*)strncat((char*)s1, (char*)s2, n);
}

// modeled using strncmp
int CLIBCALL wcsncmp(const wchar_t* s1, const wchar_t* s2, size_t n) {
  return strncmp((char*)s1, (char*)s2, n);
}

// modeled using strpbrk
#ifndef USE_CPP_OVERLOADS
wchar_t* CLIBCALL wcspbrk(const wchar_t* s1, const wchar_t* s2)
#else
const wchar_t* CLIBCALL wcspbrk(const wchar_t* s1, const wchar_t* s2) {
  return wcspbrk((wchar_t*)s1, s2);
}
wchar_t* CLIBCALL wcspbrk(wchar_t* s1, const wchar_t* s2)
#endif
{
  return (wchar_t*)strpbrk((char*)s1, (char*)s2);
}

// modeled using strchr
#ifndef USE_CPP_OVERLOADS
wchar_t* CLIBCALL wcsrchr(const wchar_t* s, wchar_t c)
#else
const wchar_t* CLIBCALL wcsrchr(const wchar_t* s, wchar_t c) {
  return wcsrchr((wchar_t*)s, c);
}

wchar_t* CLIBCALL wcsrchr(wchar_t* s, wchar_t c)
#endif
{
  return (wchar_t*)strchr((char*)s, c);
}

// modeled using strncpy
wchar_t* CLIBCALL wcsncpy(wchar_t* restrict s1,
                          const wchar_t* restrict s2,
                          size_t n) {
  return (wchar_t*)strncpy((char*)s1, (char*)s2, n);
}

// modeled using strspn
size_t CLIBCALL wcsspn(const wchar_t* s1, const wchar_t* s2) {
  return strspn((char*)s1, (char*)s2);
}

// modeled using strstr
#ifndef USE_CPP_OVERLOADS
wchar_t* CLIBCALL wcsstr(const wchar_t* s1, const wchar_t* s2)
#else

const wchar_t* CLIBCALL wcsstr(const wchar_t* s1, const wchar_t* s2) {
  return wcsstr((wchar_t*)s1, s2);
}
wchar_t* CLIBCALL wcsstr(wchar_t* s1, const wchar_t* s2)
#endif
{
  return (wchar_t*)strstr((char*)s1, (char*)s2);
}

int CLIBCALL wctob(wint_t c) { return __infer_nondet_int(); }

double CLIBCALL wcstod(const wchar_t* restrict nptr,
                       wchar_t** restrict endptr) {
  return __infer_nondet_double();
}

float CLIBCALL wcstof(const wchar_t* restrict nptr, wchar_t** restrict endptr) {
  return __infer_nondet_float();
}

// simplified modeling which returns s1
wchar_t* CLIBCALL wcstok(wchar_t* restrict s1,
                         const wchar_t* restrict s2,
                         wchar_t** restrict ptr) {
  return s1;
}

long double CLIBCALL wcstold(const wchar_t* restrict nptr,
                             wchar_t** restrict endptr) {
  return __infer_nondet_long_double();
}

long int CLIBCALL wcstol(const wchar_t* restrict nptr,
                         wchar_t** restrict endptr,
                         int base) {
  return __infer_nondet_long_int();
}

long long int CLIBCALL wcstoll(const wchar_t* restrict nptr,
                               wchar_t** restrict endptr,
                               int base) {
  return __infer_nondet_long_long_int();
}

unsigned long int CLIBCALL wcstoul(const wchar_t* restrict nptr,
                                   wchar_t** restrict endptr,
                                   int base) {
  return __infer_nondet_unsigned_long_int();
}

unsigned long long int CLIBCALL wcstoull(const wchar_t* restrict nptr,
                                         wchar_t** restrict endptr,
                                         int base) {
  return __infer_nondet_long_long_int();
}

// modeled using strncmp
size_t CLIBCALL wcsxfrm(wchar_t* restrict s1,
                        const wchar_t* restrict s2,
                        size_t n) {
  return strncmp((char*)s1, (char*)s2, n);
}

// modeled using memchr
#ifndef USE_CPP_OVERLOADS
wchar_t* CLIBCALL wmemchr(const wchar_t* s, wchar_t c, size_t n)
#else

const wchar_t* CLIBCALL wmemchr(const wchar_t* s, wchar_t c, size_t n) {
  return wmemchr((wchar_t*)s, c, n);
}
wchar_t* CLIBCALL wmemchr(wchar_t* s, wchar_t c, size_t n)
#endif
{
  return (wchar_t*)memchr((char*)s, c, n);
}

// modeled using memcmp
int CLIBCALL wmemcmp(const wchar_t* s1, const wchar_t* s2, size_t n) {
  return memcmp((char*)s1, (char*)s2, n);
}

// modeled using memcpy
wchar_t* CLIBCALL wmemcpy(wchar_t* restrict s1,
                          const wchar_t* restrict s2,
                          size_t n) {
  return (wchar_t*)memcpy((char*)s1, (char*)s2, n);
}

// modeled using memmove
wchar_t* CLIBCALL wmemmove(wchar_t* s1, const wchar_t* s2, size_t n) {
  return (wchar_t*)memmove((char*)s1, (char*)s2, n);
}

// modeled using memset
wchar_t* CLIBCALL wmemset(wchar_t* s, wchar_t c, size_t n) {
  return (wchar_t*)memset((char*)s, c, n);
}
