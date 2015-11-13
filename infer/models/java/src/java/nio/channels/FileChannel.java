/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package java.nio.channels;

import com.facebook.infer.models.InferUndefined;
import javax.annotation.Nullable;
import java.nio.channels.spi.AbstractInterruptibleChannel;
import java.nio.channels.FileLock;

public abstract class FileChannel extends AbstractInterruptibleChannel {

    public static class MapMode {

        public static MapMode PRIVATE;

        public static MapMode READ_ONLY;

        public static MapMode READ_WRITE;

        private String displayName;
    }

    public @Nullable FileLock tryLock() {
        if (InferUndefined.boolean_undefined()) {
            return null;
        } else {
            return (FileLock)InferUndefined.object_undefined();
        }
    }

    public final @Nullable FileLock tryLock(long position, long size, boolean shared) {
        return tryLock();
    }

}
