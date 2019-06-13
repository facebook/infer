/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "reachability-approved.h"

namespace Danger {
void foo();
void bar();
} // namespace Danger

namespace Ok {
void foo();
void bar();
} // namespace Ok

namespace CheckFrom {

void death_via() { death(); }

void danger_via() {
  Ok::foo();
  Danger::foo();
}

void imminent_death() { death_via(); }

void imminent_danger() { danger_via(); }

void safe() {
  good();
  Ok::foo();
  Ok::bar();
  Approved::baz();
}

struct Destructive {
  ~Destructive() { imminent_death(); }
};

} // namespace CheckFrom

void wild() {
  Danger::bar();
  CheckFrom::imminent_danger();
  CheckFrom::death_via();
}

// TODO: overrides, lambdas, passing addr of reaching fn,
