/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package dalvik.system;


public class CloseGuard {

    public static interface Reporter {
        public void report(String message, Throwable allocationSite);
    }
}
