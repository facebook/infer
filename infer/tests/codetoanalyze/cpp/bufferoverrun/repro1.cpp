/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <vector>

int __infer_nondet_int();

#define ASSERT(x) assert(x)
#define xh(h)                                        \
  ({                                                 \
    const uint64_t* u1 = (const uint64_t*)(h);       \
    const uint64_t* u2 = (const uint64_t*)((h) + 8); \
    *u1 ^ *u2;                                       \
  })
#define alo(a) aloi2(a, 0)
#define aloi2(a, i2) ((lo){i1tl(a.i), (i2)})
#define i1tl(i1) (__infer_nondet_int() ? xh((i1).h) : (i1).i)
#define HL (sizeof(char) * 16)

enum class TLOR { S, F };

uint32_t MH2(const void* k) { return (int64_t)k ^ __infer_nondet_int(); }

uint32_t th(const void* k) { return MH2(k); }

typedef int BI;
constexpr BI kBN = 0;

typedef uint64_t ft;

struct it {
  ft i{0};
  char h[HL];
  it() : i(0) { ::memset(h, '\0', sizeof(h)); }
  explicit it(ft f) : i(f) { ::memset(h, '\0', sizeof(h)); }
};

struct ai {
  it i;
  ai() {}
  ai(it i_) : i(i_) {}
};

struct lo {
  uint64_t i1;
  uint64_t i2;
};

struct lt {
  BI bI;
};

template <typename T>
struct LMB {
  TLOR lO(const lo& o) {
    auto r = TLOR::S;
    if (__infer_nondet_int()) {
      r = TLOR::F;
    }
    return r;
  }
  void u(const lo& o) {}
};

template <typename T>
struct LM {
  typedef LMB<T> B;
  void l(lt& t, const lo& o) { lI(t, o); }
  void tL(lt& t, const lo& o) { lI(t, o); }
  void u_FP(lt& t, const lo& o) {
    ASSERT(fB(o) == t.bI);
    if (t.bI == kBN) {
      return;
    }
    uI_FP(t.bI, o);
    t.bI = kBN;
  }

 private:
  BI fB(const lo& o) { return (BI)th((const void*)&o) % b.size() + 1; }
  void lI(lt& t, const lo& o) {
    auto bi = fB(o);
    auto r = b[bi - 1]->lO(o);
    if (r != TLOR::S) {
      t.bI = kBN;
      return;
    }
    t.bI = bi;
  }
  void uI_FP(BI bi, const lo& o) { b[bi - 1]->u(o); }
  std::vector<std::unique_ptr<B>> b;
};

class TFM {};

typedef TFM LMDM;

static LM<LMDM>* al;

static inline void ral_FP(lt* t, ai a) {
  ASSERT(t);
  lo o = alo(a);
  al->u_FP(*t, o);
}

static inline void gal(lt* t, ai a) {
  ASSERT(t);
  lo o = alo(a);
  if (__infer_nondet_int()) {
    al->tL(*t, o);
  } else {
    al->l(*t, o);
  }
}

inline ai aft(ft i) { return ai(it(i)); }

struct im {
 private:
  char e[];

 public:
  const char* gKPC() const noexcept { return e; }
};

struct arh {
  ft i1;
};

static void am_Good(im* it) {
  const arh* ch = (const arh*)it->gKPC();
  const ai a = aft(ch->i1);
  lt at;
  gal(&at, a);
  ral_FP(&at, a);
}
