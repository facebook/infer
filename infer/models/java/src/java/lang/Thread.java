/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
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
