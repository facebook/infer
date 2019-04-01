/*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace Danger {
void foo();
void bar();
} // namespace Danger

void death();

void good();

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
}

} // namespace CheckFrom

void wild() {
  Danger::bar();
  CheckFrom::imminent_danger();
  CheckFrom::death_via();
}

// TODO: overrides, lambdas, passing addr of reaching fn,
