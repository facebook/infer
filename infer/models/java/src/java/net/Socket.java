/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.net;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.facebook.infer.builtins.InferBuiltins;

public class Socket {

    SocketImpl impl;

    public Socket() {
        try {
            setImpl();
        } catch (IOException e) {
        }
    }

    void setImpl() throws IOException {
        impl = new PlainSocketImpl();
    }

    public Socket(Proxy proxy) {
        this();
    }

    protected Socket(SocketImpl impl) throws SocketException {
        this();
    }

    public Socket(String host, int port) throws UnknownHostException,
            IOException {
        this();
    }

    public Socket(InetAddress address, int port) throws IOException {
        this();
    }

    public Socket(String host, int port, InetAddress localAddr, int localPort)
            throws IOException {
        this();
    }

    public Socket(InetAddress address, int port, InetAddress localAddr,
                  int localPort) throws IOException {
        this();
    }

    public Socket(String host, int port, boolean stream) throws IOException {
        this();
    }

    public Socket(InetAddress host, int port, boolean stream)
            throws IOException {
        this();
    }

    public InputStream getInputStream() throws IOException {
      InferBuiltins.__check_untainted(this);
      return ((PlainSocketImpl) impl).getInputStream();
    }

    public OutputStream getOutputStream() throws IOException {
      InferBuiltins.__check_untainted(this);
      return ((PlainSocketImpl) impl).getOutputStream();
    }

    public synchronized void close() throws IOException {
        ((PlainSocketImpl) impl).close();
    }

}
