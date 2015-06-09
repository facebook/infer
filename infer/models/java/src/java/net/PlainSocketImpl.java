/*
 * Copyright (c) 1995, 2011, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package java.net;

import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;
import dalvik.system.CloseGuard;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;


class PlainSocketImpl extends SocketImpl {
    private static InetAddress lastConnectedAddress;

    private static int lastConnectedPort;

    private boolean streaming = true;

    private boolean shutdownInput;

    private Proxy proxy;

    private CloseGuard guard;

    PlainSocketImpl() {
        guard = new CloseGuard();
        InferBuiltins.__set_file_attribute(guard);
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
        InferBuiltins.__set_mem_attribute(guard);
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
