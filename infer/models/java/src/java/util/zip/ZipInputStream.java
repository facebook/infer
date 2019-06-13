/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferUndefined;
import java.io.IOException;

public class ZipInputStream {

  public ZipEntry getNextEntry() throws IOException {
    boolean undef = InferUndefined.boolean_undefined();
    if (undef) {
      return new ZipEntry("");
    } else throw new IOException();
  }

  public void closeEntry() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }
}
