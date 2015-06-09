package com.squareup.okhttp.internal;

import com.facebook.infer.models.InferCloseables;

import java.io.Closeable;
import java.io.IOException;
import java.nio.charset.Charset;


public class Util {

    public static final Charset US_ASCII = Charset.forName("US-ASCII");

    public static void closeQuietly(Closeable closeable) throws IOException {
        InferCloseables.closeQuietly(closeable);
    }

}
