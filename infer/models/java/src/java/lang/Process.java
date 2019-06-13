/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import com.facebook.infer.builtins.InferBuiltins;
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
