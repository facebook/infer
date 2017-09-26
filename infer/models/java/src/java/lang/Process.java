/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.lang;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

import java.io.FileDescriptor;

public class Process {

  public Process(int pid, FileDescriptor in, FileDescriptor out, FileDescriptor err) {
    InferBuiltins.__set_file_attribute(this);
  }

  public void destroy() {
    InferBuiltins.__set_mem_attribute(this);
  }

  public Process destroyForcibly() {
    destroy();
    return this;
  }

}
