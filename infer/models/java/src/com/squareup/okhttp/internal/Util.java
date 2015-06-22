/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package com.squareup.okhttp.internal;

import com.facebook.infer.models.InferCloseables;

import java.io.Closeable;
import java.io.IOException;


public class Util {

    public static void closeQuietly(Closeable closeable) throws IOException {
        InferCloseables.closeQuietly(closeable);
    }

}
