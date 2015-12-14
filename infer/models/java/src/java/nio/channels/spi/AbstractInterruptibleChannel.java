/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
