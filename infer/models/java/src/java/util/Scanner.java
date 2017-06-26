/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util;

import com.facebook.infer.builtins.InferUndefined;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.lang.IllegalArgumentException;


public class Scanner {

  InputStream src;
  private void init(InputStream source) {
    src = source;
  }
  public Scanner(InputStream source) {
    init(source);
  }

  public Scanner(InputStream source, String charsetName) throws IllegalArgumentException {
    if (InferUndefined.boolean_undefined()) {
      init(source);
    } else {
      throw new IllegalArgumentException();
    }
  }

  public Scanner(File source) throws FileNotFoundException {
    init(new FileInputStream(source));
  }

  public Scanner(File source, String charsetName)
    throws FileNotFoundException, IllegalArgumentException {
    if (InferUndefined.boolean_undefined()) {
      init(new FileInputStream(source));
    } else {
      throw new IllegalArgumentException();
    }
  }

  public void close() {
    try {
      if (src != null) {
        src.close();
      }
    } catch (IOException e) {

    }
  }
}
