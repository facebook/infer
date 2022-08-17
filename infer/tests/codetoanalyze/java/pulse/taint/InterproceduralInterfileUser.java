/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

public class InterproceduralInterfileUser {
  String name;

  InterproceduralInterfileUser(String name) {
    name = name;
  }

  int getPhoneNumber() {
    return 42;
  }
}
