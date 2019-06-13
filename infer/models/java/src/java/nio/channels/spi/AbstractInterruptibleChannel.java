/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.nio.channels.spi;

import java.io.IOException;
import java.nio.FileChannelImpl;

public class AbstractInterruptibleChannel {

  public final void close() throws IOException {
    if (this instanceof FileChannelImpl) {
      ((FileChannelImpl) this).implCloseChannel();
    }
  }
}
