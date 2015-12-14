/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.lang;

public class Thread implements Runnable {

    Runnable target;

    public static interface UncaughtExceptionHandler {
    }

    public synchronized void start() {
        run();
    }

    public void run() {
        if (target != null) {
            target.run();
        }
    }

}
