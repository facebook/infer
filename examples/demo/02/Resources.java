/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

class Resources {

  public static void cat() throws IOException {
    FileInputStream infile = null;
    FileOutputStream outfile = null;
    try {
      infile = new FileInputStream(new File("infile.txt"));
      outfile = new FileOutputStream(new File("outfile.txt"));
      outfile.write(infile.read());
    } finally {
      if (infile != null) infile.close();
      if (outfile != null) outfile.close();
    }
  }
}
