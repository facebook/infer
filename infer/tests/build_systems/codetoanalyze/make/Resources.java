/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package hello;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class Resources {

  public static FileOutputStream allocateResource() {
    try {
      File file = new File("foo.txt");
      return new FileOutputStream(file);
    } catch (IOException e) {
      return null;
    }
  }
}
