/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package javax.net.ssl;

import com.facebook.infer.models.InferBuiltins;

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
