/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

public class Client<T> {

  String foo() {
    Library<T> lib = new Library<T>();
    T t = lib.get();
    return t == null ? "default" : t.toString();
  }
}
