/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import javax.inject.*;

class ProviderTest {
  /* Assume that injecting A is expensive, on the other hand injecting Integer is cheap. */
  class A {}

  @Inject private Provider<A> mProviderA;
  @Inject private Provider<Integer> mProviderInteger;

  void use_provided_A(A a) {}

  void use_provided_Integer(Integer i) {}

  void expensive_get_A_constant() {
    use_provided_A(mProviderA.get());
  }

  void cheap_get_Integer_linear() {
    use_provided_Integer(mProviderInteger.get());
  }
}
