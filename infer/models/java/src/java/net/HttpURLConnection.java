/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.net;

import com.facebook.infer.builtins.InferBuiltins;

public class HttpURLConnection extends URLConnection {

  public HttpURLConnection(URL url) {
    super(url);
    InferBuiltins.__set_file_attribute(this);
  }

  public void disconnect() {
    InferBuiltins.__set_mem_attribute(this);
  }
}
