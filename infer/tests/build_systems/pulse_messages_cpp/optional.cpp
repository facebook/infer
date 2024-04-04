/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <optional>

namespace optional_empty_access {
int intraprocedural_bad() {
  std::optional<int> foo;
  return foo.value();
}

std::optional<int> return_empty() { return {}; }

int empty_via_return_bad() {
  std::optional<int> o = return_empty();
  return o.value();
}

void access(std::optional<int> x) { int _ = x.value(); }

void via_access_bad() {
  std::optional<int> o;
  access(o);
}

void empty_via_return_via_access_bad() {
  std::optional<int> p = return_empty();
  access(p);
}

void latent(int a) {
  if (a == 4) {
    std::optional<int> foo;
    int _ = foo.value();
  }
}

void propagate_latent_1_latent(int a1) { latent(a1); }

void propagate_latent_2_latent(int a2) { propagate_latent_1_latent(a2); }

void propagate_latent_3_latent(int a3) { propagate_latent_2_latent(a3); }

void make_latent_manifest() { propagate_latent_3_latent(4); }
} // namespace optional_empty_access
