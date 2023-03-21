/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

/**
 * To test taint config based a class name and method return type. In that case only methods
 * returning `String` are considered as sources.
 */
public class InferBaseSource {

  public String inferBaseSecretSource() {
    return "secret";
  }

  public Object inferBaseNotSource() {
    return new Object();
  }
}
