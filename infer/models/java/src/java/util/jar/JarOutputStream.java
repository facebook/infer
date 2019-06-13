/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.jar;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;

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
