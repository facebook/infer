/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package java.nio.channels;

import com.facebook.infer.models.InferBuiltins;
import java.nio.channels.spi.AbstractInterruptibleChannel;
import java.nio.channels.FileLock;

public abstract class FileChannel extends AbstractInterruptibleChannel {

    public static class MapMode {

        public static MapMode PRIVATE;

        public static MapMode READ_ONLY;

        public static MapMode READ_WRITE;

        private String displayName;
    }

    private native FileLock getFileLock();

    FileLock lock() {
        FileLock f = getFileLock();
        InferBuiltins.assume(f != null);
        return f;
    }
}
