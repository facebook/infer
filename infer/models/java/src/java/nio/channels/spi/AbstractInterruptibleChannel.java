/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
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
