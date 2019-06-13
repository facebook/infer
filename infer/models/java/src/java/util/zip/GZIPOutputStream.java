/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferUndefined;
import java.io.IOException;
import java.io.OutputStream;

public class GZIPOutputStream extends DeflaterOutputStream {

  public GZIPOutputStream(OutputStream out, int size) throws IOException {
    super(out);
    if (!InferUndefined.boolean_undefined()) {
      throw new IOException();
    }
  }

  public GZIPOutputStream(OutputStream out) throws IOException {
    super(out);
    if (!InferUndefined.boolean_undefined()) {
      throw new IOException();
    }
  }
}
