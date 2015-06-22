/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.net;

import com.facebook.infer.models.InferUndefined;

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
