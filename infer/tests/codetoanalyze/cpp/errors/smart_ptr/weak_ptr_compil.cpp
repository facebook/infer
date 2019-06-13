/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <functional>
#include <map>
#include <memory>
#include <set>
#include <unordered_map>

/* Compilation tests */

namespace weak_ptr_lock_repro_small {
template <class T>
std::shared_ptr<T> joinT(std::shared_ptr<T> x) {
  return x;
};

void foo(std::weak_ptr<int> p) {
  auto self = p.lock();
  std::shared_ptr<int> x = joinT(self);
}
} // namespace weak_ptr_lock_repro_small

namespace weak_ptr_lock_repro_large {

class S {
 public:
  template <typename T>
  std::shared_ptr<T> joinT(std::shared_ptr<T> s);
};

class DCC {
 public:
  const std::shared_ptr<S>& s();
};

class DC {};

class CDM {
 public:
  std::shared_ptr<DC> gdc(std::function<DCC()>);
};

class RDC : DC {
 public:
  static std::shared_ptr<RDC> create(std::function<DCC()> cf);

 private:
  const std::shared_ptr<CDM> cdm;
  mutable std::function<std::shared_ptr<DC>()> dcf;
};

std::shared_ptr<RDC> RDC::create(std::function<DCC()> cf) {
  auto dc = std::make_shared<RDC>();
  dc->dcf = [cf = std::move(cf),
             weakSelf =
                 std::weak_ptr<RDC>(dc)]() mutable -> std::shared_ptr<DC> {
    if (auto self = weakSelf.lock()) {
      return self->cdm->gdc([&]() mutable {
        auto c = cf();
        c.s()->joinT(self);
        return c;
      });
    }
    return nullptr;
  };
  return dc;
}
} // namespace weak_ptr_lock_repro_large

namespace weak_ptr_owner_less {
class K {};
class V {};
class C {
  using S = std::set<std::weak_ptr<K>, std::owner_less<std::weak_ptr<K>>>;
  std::
      map<std::weak_ptr<K>, std::weak_ptr<V>, std::owner_less<std::weak_ptr<K>>>
          m;
  S s;
#ifdef INFER_USE_LIBCPP
  /* requires Clang headers */
  std::unordered_map<K, S> u;
#endif
};
} // namespace weak_ptr_owner_less
