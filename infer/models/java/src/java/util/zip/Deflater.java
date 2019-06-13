/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferBuiltins;

public class Deflater {

  public Deflater() {
    InferBuiltins.__set_file_attribute(this);
  }

  public Deflater(int level) {
    this();
  }

  public Deflater(int level, boolean nowrap) {
    this();
  }

  public void end() {
    InferBuiltins.__set_mem_attribute(this);
  }
}
