/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.net;

import javax.net.ssl.HttpsURLConnection;
import java.util.Hashtable;
import com.facebook.infer.builtins.InferBuiltins;

public final class URL implements java.io.Serializable {

    private String protocol;
    private String host;
    private int port;
    private String file;

    public URL(String protocol, String host, int port, String file) throws MalformedURLException {
        this(protocol, host, port, file, null);
    }


    public URL(String protocol, String host, String file) throws MalformedURLException {
        this(protocol, host, -1, file);
    }


    public URL(String protocol, String host, int port, String file,
               URLStreamHandler handler) throws MalformedURLException {
        this.protocol = protocol;
        this.host = host;
        this.file = file;
        this.port = port;
    }

    public URL(String spec) throws MalformedURLException {
        this(null, spec);
    }

    public URL(URL context, String spec) throws MalformedURLException {
        this(context, spec, null);
    }

    public URL(URL context, String spec, URLStreamHandler handler) throws MalformedURLException {
        protocol = spec;
    }


    public URLConnection openConnection(Proxy proxy) throws java.io.IOException {
        if (protocol == "jar") {
            return new JarURLConnection(this);
        } else if (protocol == "http") {
            return new HttpURLConnection(this);
        } else if (protocol == "https") {
            return new HttpsURLConnection(this);
        } else
            return new URLConnection(this);
    }

    public URLConnection openConnection() throws java.io.IOException {
        return openConnection(null);
    }

}
