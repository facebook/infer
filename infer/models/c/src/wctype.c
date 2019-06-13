/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// modelling of wctype functions

#define _DONT_USE_CTYPE_INLINE_
#include <wctype.h>

#include "infer_builtins.h"

#define restrict

#ifdef _WIN32
#define CLIBCALL __cdecl
#else
#define CLIBCALL
#endif

// Microsoft-specific
int CLIBCALL __iswcsymf(wint_t wc) { return __infer_nondet_int(); }

// Microsoft-specific, inline
int CLIBCALL isleadbyte(int wc) { return __infer_nondet_int(); }

int CLIBCALL iswalnum(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswalpha(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswblank(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswcntrl(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswctype(wint_t wc, wctype_t desc) { return __infer_nondet_int(); }

int CLIBCALL iswdigit(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswgraph(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswlower(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswprint(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswpunct(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswspace(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswupper(wint_t wc) { return __infer_nondet_int(); }

int CLIBCALL iswxdigit(wint_t wc) { return __infer_nondet_int(); }

wint_t CLIBCALL towlower(wint_t wc) { return __infer_nondet_int(); }

wint_t CLIBCALL towctrans(wint_t wc, wctrans_t desc) {
  return __infer_nondet_int();
}

wint_t CLIBCALL towupper(wint_t wc) { return __infer_nondet_int(); }

wctrans_t CLIBCALL wctrans(const char* property) {
  return (wctrans_t)__infer_nondet_int();
}

wctype_t CLIBCALL wctype(const char* property) {
  return (wctype_t)__infer_nondet_int();
}
