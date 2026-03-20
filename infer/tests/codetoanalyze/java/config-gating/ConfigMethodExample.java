/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.configgating;

/**
 * Tests config-gating detection for instance method patterns, where the config is identified by an
 * integer constant passed as an argument.
 */
public class ConfigMethodExample {

  static class Config {
    boolean isEnabled(int code) {
      return false;
    }

    int getValue(int code) {
      return 0;
    }
  }

  Config config = new Config();

  static void doSomething() {}

  static void doSomethingElse() {}

  // Direct if-check on isEnabled
  void direct_check() {
    if (config.isEnabled(42)) {
      doSomething();
    }
  }

  // Store result in variable, then check
  void stored_check() {
    boolean enabled = config.isEnabled(99);
    if (enabled) {
      doSomething();
    }
  }

  // Negated check
  void negated_check() {
    if (!config.isEnabled(42)) {
      doSomething();
    }
  }

  // Not gated
  void not_gated() {
    doSomething();
  }
}
