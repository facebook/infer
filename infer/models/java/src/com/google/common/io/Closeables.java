package com.google.common.io;

import com.facebook.infer.models.InferCloseables;
import com.facebook.infer.models.InferUndefined;

import java.io.Closeable;
import java.io.IOException;

public final class Closeables {

    public static void close(Closeable closeable, boolean swallowIOException) throws IOException {
        InferCloseables.close(closeable, swallowIOException);
        if (!swallowIOException)
            InferUndefined.can_throw_ioexception_void();
    }

    public static void closeQuietly(Closeable closeable) {
        InferCloseables.closeQuietly(closeable);
    }

}