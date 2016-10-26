/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package javax.net.ssl;

import com.facebook.infer.builtins.InferBuiltins;

import java.net.HttpURLConnection;
import java.net.URL;

public class HttpsURLConnection extends HttpURLConnection {

    /**
     * Creates an <code>HttpsURLConnection</code> using the
     * URL specified.
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
