/*
 * Copyright (C) 2012 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.squareup.okhttp.internal;

import com.facebook.infer.models.InferUndefined;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

public class StrictLineReader implements Closeable {

    private InputStream in;
    private Charset charset;
    private byte[] buf;

    public StrictLineReader(InputStream in, Charset charset) {
        this(in, 8192, charset);
    }

    public StrictLineReader(InputStream in, int capacity, Charset charset) {
        if (in == null) {
            throw new NullPointerException();
        }
        if (capacity < 0) {
            throw new IllegalArgumentException("capacity <= 0");
        }

        this.in = in;
        this.charset = charset;
        buf = new byte[capacity];
    }

    public void close() throws IOException {
        in.close();
        InferUndefined.can_throw_ioexception_void();
    }

    public String readLine() throws IOException {
        return InferUndefined.can_throw_ioexception_string();
    }


    public int readInt() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }


}

