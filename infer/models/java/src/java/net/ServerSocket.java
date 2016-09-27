/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.net;

import com.facebook.infer.builtins.InferUndefined;

import java.io.IOException;

public class ServerSocket {

    private SocketImpl impl;

    public ServerSocket() throws IOException {
        impl = new PlainSocketImpl();
    }

    public ServerSocket(int port) throws IOException {
        this();
    }

    public ServerSocket(int port, int backlog) throws IOException {
        this();
    }

    public ServerSocket(int port, int backlog, InetAddress bindAddr) throws IOException {
        this();
    }

    public void close() throws IOException {
        ((PlainSocketImpl) impl).close();
    }

    public Socket accept() throws IOException {
        if (InferUndefined.boolean_undefined()) {
            Socket s = new Socket((SocketImpl) null);
            return s;
        } else throw new IOException();
    }

}
