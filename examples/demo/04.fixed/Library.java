/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

public class Library<T> {

  @Nullable T t;

  private native @Nullable T mayReturnNull();

  public Library() {
    t = mayReturnNull();
  }

  @Nullable
  public T get() {
    return t;
  }
}
