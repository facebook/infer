/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.NoSuchElementException;

public class ZipFile {

  private void init() throws IOException {
    InferUndefined.can_throw_ioexception_void();
    InferBuiltins.__set_file_attribute(this);
  }

  public ZipFile(String name) throws IOException {
    init();
  }

  public ZipFile(File file, int mode) throws IOException {
    init();
  }

  public ZipFile(File file) throws ZipException, IOException {
    init();
  }

  public void close() throws IOException {
    InferBuiltins.__set_mem_attribute(this);
    InferUndefined.can_throw_ioexception_void();
  }

  protected void finalize() throws IOException {
    close();
  }

  public Enumeration<? extends ZipEntry> entries() {

    return new Enumeration<ZipEntry>() {
      private boolean hasEls;

      public boolean hasMoreElements() {
        return hasEls;
      }

      public ZipEntry nextElement() throws NoSuchElementException {
        if (hasEls)
          return new ZipEntry("");
        else
          throw new NoSuchElementException();
      }
    };
  }

}
