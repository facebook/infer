/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.nio.channels;

import java.nio.channels.spi.AbstractInterruptibleChannel;

public abstract class FileChannel extends AbstractInterruptibleChannel {

    public static class MapMode {

        public static MapMode PRIVATE;

        public static MapMode READ_ONLY;

        public static MapMode READ_WRITE;

        private String displayName;
    }
}
