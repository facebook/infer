package com.facebook.infer.models;

import com.squareup.okhttp.internal.StrictLineReader;

import java.io.*;

public final class InferCloseables {

    private InferCloseables() {
    }

    public static void close(Closeable closeable, boolean swallowIOException)
            throws IOException {
        if (closeable == null) return;
        if (closeable instanceof InputStream) {
            ((InputStream) closeable).close();
        } else if (closeable instanceof OutputStream) {
            ((OutputStream) closeable).close();
        } else if (closeable instanceof Reader) {
            ((Reader) closeable).close();
        } else if (closeable instanceof Writer) {
            ((Writer) closeable).close();
        } else if (closeable instanceof StrictLineReader) {
            ((StrictLineReader) closeable).close();
        }
    }

    public static void closeQuietly(Closeable closeable) {
        try {
            close(closeable, true);
        } catch (IOException e) {
        }
    }

}
