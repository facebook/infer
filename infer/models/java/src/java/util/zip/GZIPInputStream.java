/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.io.IOException;
import java.io.InputStream;

public class GZIPInputStream {

  public GZIPInputStream(InputStream in) throws IOException {
    InferUndefined.can_throw_ioexception_void();
    InferBuiltins.__set_mem_attribute(in);
    InferBuiltins.__set_file_attribute(this);
  }

  public GZIPInputStream(InputStream in, int size) throws IOException {
    this(in);
  }
}
