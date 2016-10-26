/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferBuiltins;

public class Inflater {

  public Inflater() {
    InferBuiltins.__set_file_attribute(this);
  }

  public Inflater(boolean nowrap) {
    this();
  }

  public void end() {
    InferBuiltins.__set_mem_attribute(this);
  }

}
