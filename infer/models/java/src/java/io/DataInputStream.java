/*
 * Copyright (c) 1994, 2006, Oracle and/or its affiliates. All rights reserved.
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

package java.io;

import com.facebook.infer.models.InferUndefined;

public class DataInputStream extends FilterInputStream {

    private byte[] scratch;

    public DataInputStream(InputStream in) {
        super(in);
    }

    public int read(byte b[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[], int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public final boolean readBoolean() throws IOException {
        return InferUndefined.can_throw_ioexception_boolean();
    }

    public final byte readByte() throws IOException {
        return InferUndefined.can_throw_ioexception_byte();
    }

    public final char readChar() throws IOException {
        return InferUndefined.can_throw_ioexception_char();
    }

    public final double readDouble() throws IOException {
        return InferUndefined.can_throw_ioexception_double();
    }

    public final float readFloat() throws IOException {
        return InferUndefined.can_throw_ioexception_float();
    }

    public final void readFully(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void readFully(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final int readInt() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public final long readLong() throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public final short readShort() throws IOException {
        return InferUndefined.can_throw_ioexception_short();
    }

    public final int readUnsignedByte() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public final int readUnsignedShort() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public final String readUTF() throws IOException {
        return InferUndefined.can_throw_ioexception_string();
    }

    public static final String readUTF(DataInput in) throws IOException {
        return InferUndefined.can_throw_ioexception_string();
    }

    public final int skipBytes(int n) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

}
