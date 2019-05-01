/*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

public class ResourcesFixed {

  public static void cat() throws IOException {
    try (FileInputStream fis = new FileInputStream(new File("infile.txt"));
        FileOutputStream fos = new FileOutputStream(new File("outfile.txt")); ) {
      fos.write(fis.read());
    }
  }
}
