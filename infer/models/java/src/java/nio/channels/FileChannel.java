/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.nio.channels;

import com.facebook.infer.builtins.InferUndefined;
import java.io.IOException;
import java.nio.channels.spi.AbstractInterruptibleChannel;
import javax.annotation.Nullable;

public abstract class FileChannel extends AbstractInterruptibleChannel {

  public static class MapMode {

    public static MapMode PRIVATE;

    public static MapMode READ_ONLY;

    public static MapMode READ_WRITE;

    private String displayName;
  }

  @Nullable
  FileLock tryLock() throws IOException {
    InferUndefined.can_throw_ioexception_object();
    if (InferUndefined.boolean_undefined()) {
      return null;
    } else {
      return (FileLock) InferUndefined.object_undefined();
    }
  }

  @Nullable
  FileLock tryLock(long position, long size, boolean shared) throws IOException {
    return tryLock();
  }
}
