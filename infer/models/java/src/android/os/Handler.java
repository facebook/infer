/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.os;

import com.facebook.infer.models.InferUndefined;

class Handler {

    // model queue of tasks using 1-recency abstraction
    private static Runnable sFakeHandlerQueue;

    public final boolean postDelayed(Runnable r, long delayMillis) {
        // model posting a delayed message as keeping a persistent reference to the Runnable
        sFakeHandlerQueue = r;
        return InferUndefined.boolean_undefined();
    }

    public final void removeCallbacks(Runnable r) {
        if (r == sFakeHandlerQueue)
            sFakeHandlerQueue = null;
    }

    public final void removeCallbacks(Runnable r, Object token) {
        removeCallbacks(r);
    }

    public final void removeCallbacksAndMessages(Object token) {
        sFakeHandlerQueue = null;
    }
}
