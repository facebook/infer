/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.lang;

import java.io.File;
import java.io.IOException;


public class Runtime {

  private Runtime() {
  }

  public Process exec(String command) throws IOException {
    return exec(command, null, null);
  }

  public Process exec(String command, String[] envp) throws IOException {
    return exec(command, envp, null);
  }

  public Process exec(String command, String[] envp, File dir)
    throws IOException {
    return ProcessManager.getInstance().exec(null, envp, null, false);
  }

  public Process exec(String cmdarray[]) throws IOException {
    return exec(cmdarray, null, null);
  }

  public Process exec(String[] cmdarray, String[] envp) throws IOException {
    return exec(cmdarray, envp, null);
  }

  public Process exec(String[] cmdarray, String[] envp, File dir)
    throws IOException {
    return ProcessManager.getInstance().exec(cmdarray, envp, dir, false);
  }

}
