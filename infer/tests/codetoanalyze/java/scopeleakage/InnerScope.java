/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

public class InnerScope {
  public static <T> Box<T> getBox(Class<?> c) {
    return new Box<T>(c.getName());
  }
}
