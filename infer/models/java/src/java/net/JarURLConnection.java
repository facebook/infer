/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.net;

public class JarURLConnection extends URLConnection {

    protected JarURLConnection(URL url) throws MalformedURLException {
        super(url);
    }
}
