/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package lib;

import java.io.Closeable;

public class MyStream implements Closeable {

    String file;

    public MyStream(String s) {
        this.file = s;
    }

    public String readContent() {
        return this.file;
    }

    public void close () {
        this.file = null;
    }
    
}
