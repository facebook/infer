/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import java.io.File;
import java.io.IOException;

public class Runtime {

  private Runtime() {}

  public Process exec(String command) throws IOException {
    return exec(command, null, null);
  }

  public Process exec(String command, String[] envp) throws IOException {
    return exec(command, envp, null);
  }

  public Process exec(String command, String[] envp, File dir) throws IOException {
    return ProcessManager.getInstance().exec(null, envp, null, false);
  }

  public Process exec(String cmdarray[]) throws IOException {
    return exec(cmdarray, null, null);
  }

  public Process exec(String[] cmdarray, String[] envp) throws IOException {
    return exec(cmdarray, envp, null);
  }

  public Process exec(String[] cmdarray, String[] envp, File dir) throws IOException {
    return ProcessManager.getInstance().exec(cmdarray, envp, dir, false);
  }
}
