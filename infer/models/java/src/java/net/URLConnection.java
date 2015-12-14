/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.net;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;


public class URLConnection {

    protected URL url;

    public URLConnection(URL url) {
        this.url = url;
    }

    public URL getURL() {
        return url;
    }

    public InputStream getInputStream() throws IOException {
        if (this instanceof HttpURLConnection) {
            byte[] arr = {1};
            return new ByteArrayInputStream(arr);
        } else return new FileInputStream("");
    }

    public OutputStream getOutputStream() throws IOException {
        if (this instanceof HttpURLConnection) {
            return new ByteArrayOutputStream();
        } else return new FileOutputStream("");
    }
}
