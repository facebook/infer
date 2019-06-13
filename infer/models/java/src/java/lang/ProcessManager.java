/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import com.facebook.infer.builtins.InferUndefined;
import java.io.File;
import java.io.FileDescriptor;
import java.io.IOException;

abstract class ProcessManager {

  public Process exec(
      String[] taintedCommand,
      String[] taintedEnvironment,
      File workingDirectory,
      boolean redirectErrorStream)
      throws IOException {

    FileDescriptor in = new FileDescriptor();
    FileDescriptor out = new FileDescriptor();
    FileDescriptor err = new FileDescriptor();

    return new Process(InferUndefined.int_undefined(), in, out, err);
  }

  public static native ProcessManager getInstance();
}
