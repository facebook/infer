/*
 * Copyright 2012-present Facebook, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 */

package utils;

import org.junit.rules.TemporaryFolder;

import java.util.Map;

import javax.annotation.Nullable;


public class DebuggableTemporaryFolder extends TemporaryFolder {

  private
  @Nullable
  String name;
  private boolean doNotDeleteOnExit;

  public DebuggableTemporaryFolder() {
    Map<String, String> env = System.getenv();
    if (env.get("INFER_KEEP_FOLDER") != null) {
      doNotDeleteOnExit = true;
    }
  }

  /**
   * If invoked, the directory created by this {@link org.junit.rules.TemporaryFolder} will not be
   * deleted when the test finishes.
   *
   * @return {@code this}
   */
  public DebuggableTemporaryFolder doNotDeleteOnExit() {
    this.doNotDeleteOnExit = true;
    return this;
  }

  /**
   * Name to use to identify this {@link org.junit.rules.TemporaryFolder} when writing log messages
   * to stdout.
   *
   * @return {@code this}
   */
  public DebuggableTemporaryFolder setName(String name) {
    this.name = name;
    return this;
  }

  @Override
  public void after() {
    if (doNotDeleteOnExit) {
      String name = this.name == null ? "TemporaryFolder" : this.name;
      System.out.printf("%s available at %s.\n", name, getRoot());
    } else {
      super.after();
    }
  }
}
