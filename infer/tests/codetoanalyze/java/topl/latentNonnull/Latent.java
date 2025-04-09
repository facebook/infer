/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
public abstract class Latent {

  // This is a latent issue because it depends on the caller whether x is null
  void test_Latent(Object x) {
    f(x);
  }

  // This is currently a latent issue that gets discarded. It's latent because it depends on whether
  // g() returns null; it gets discarded because the caller has no say in whether g() returns null
  // or not. Ideally, we'd like this to be a manifest issue, because the condition "something is
  // nonnull" should be optimistically considered to be true (unless proven otherwise).
  void fn_test_Bad() {
    g();
  }

  abstract void f(Object _x);

  abstract Object g();
}
