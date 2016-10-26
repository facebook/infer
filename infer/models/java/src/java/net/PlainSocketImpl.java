/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.net;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;


class PlainSocketImpl extends SocketImpl {

    PlainSocketImpl() {
        InferBuiltins.__set_file_attribute(this);
    }


    protected void create(boolean stream) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


    protected void connect(String host, int port) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


    protected void connect(InetAddress address, int port) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


    protected void connect(SocketAddress address, int timeout) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


    protected void bind(InetAddress host, int port) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


    protected void listen(int backlog) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


    protected void accept(SocketImpl s) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public synchronized InputStream getInputStream() throws IOException {
        return new PlainSocketInputStream(this);
    }

    public synchronized OutputStream getOutputStream() throws IOException {
        return new PlainSocketOutputStream(this);
    }


    protected int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    protected void close() throws IOException {
        InferBuiltins.__set_mem_attribute(this);
        InferUndefined.can_throw_ioexception_void();
    }

    protected void finalize() throws IOException {
        close();
    }

    void socketCreate(boolean isServer) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    void socketConnect(InetAddress address, int port, int timeout) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    void socketBind(InetAddress address, int port) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    void socketListen(int count) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


    void socketAccept(SocketImpl s) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    int socketAvailable() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    void socketClose0(boolean useDeferredClose) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    void socketShutdown(int howto) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    void socketSetOption(int cmd, boolean on, Object value) throws SocketException {
        InferUndefined.can_throw_socketexception_void();
    }

    int socketGetOption(int opt, Object iaContainerObj) throws SocketException {
        return InferUndefined.can_throw_socketexception_int();
    }

    void socketSendUrgentData(int data) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public Object getOption(int opt) throws SocketException {
        return InferUndefined.can_throw_socketexception_object();
    }

    public void setOption(int opt, Object val) throws SocketException {
        InferUndefined.can_throw_socketexception_void();
    }

    protected void sendUrgentData(int data) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    private static class PlainSocketInputStream extends InputStream {
        private PlainSocketImpl socketImpl;

        public PlainSocketInputStream(PlainSocketImpl socketImpl) {
            this.socketImpl = socketImpl;
        }

        public int available() throws IOException {
            return InferUndefined.can_throw_ioexception_int();
        }

        public void close() throws IOException {
            socketImpl.close();
        }

        public int read() throws IOException {
            return InferUndefined.can_throw_ioexception_int();
        }

        public int read(byte[] buffer, int byteOffset, int byteCount) throws IOException {
            return InferUndefined.can_throw_ioexception_int();
        }
    }

    private static class PlainSocketOutputStream extends OutputStream {
        private PlainSocketImpl socketImpl;

        public PlainSocketOutputStream(PlainSocketImpl socketImpl) {
            this.socketImpl = socketImpl;
        }

        public void close() throws IOException {
            socketImpl.close();
        }

        public void write(int oneByte) throws IOException {
            InferUndefined.can_throw_ioexception_void();
        }

        public void write(byte[] buffer, int offset, int byteCount) throws IOException {
            InferUndefined.can_throw_ioexception_void();
        }
    }

}
