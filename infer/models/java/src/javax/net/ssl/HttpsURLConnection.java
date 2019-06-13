/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package javax.net.ssl;

import com.facebook.infer.builtins.InferBuiltins;
import java.net.HttpURLConnection;
import java.net.URL;

public class HttpsURLConnection extends HttpURLConnection {

  /**
   * Creates an <code>HttpsURLConnection</code> using the URL specified.
   *
   * @param url the URL
   */
  public HttpsURLConnection(URL url) {
    super(url);
  }

  public void disconnect() {
    InferBuiltins.__set_mem_attribute(this);
  }
}
