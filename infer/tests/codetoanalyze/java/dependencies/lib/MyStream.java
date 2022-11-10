/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package lib;

import java.io.Closeable;

public class MyStream implements Closeable {

  Str file;

  public MyStream(Str s) {
    this.file = s;
  }

  public Str readContent() {
    return this.file;
  }

  public void close() {
    this.file = null;
  }
}
