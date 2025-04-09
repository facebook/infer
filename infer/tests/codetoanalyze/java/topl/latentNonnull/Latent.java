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

  // Found only if --topl-nonnull-soft given.
  void test_Bad() {
    g();
  }

  abstract void f(Object _x);

  abstract Object g();
}
