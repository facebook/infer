/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util.jar;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;


public class JarOutputStream {

  public JarOutputStream(OutputStream out, Manifest man) throws IOException {
    this(out);
  }

  public JarOutputStream(OutputStream out) throws IOException {
    InferUndefined.can_throw_ioexception_void();
    InferBuiltins.__set_mem_attribute(out);
    InferBuiltins.__set_file_attribute(this);
  }

  public void putNextEntry(ZipEntry ze) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

}
