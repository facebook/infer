/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

class Thread implements Runnable {

  public native void run();

  /* This is a coarse model which doesn't. e.g., explicitly
     keep track of when something has been locked or unlocked.
     The typical use case for this method, though, is for when
     you don't know if a lock is held, we simply want that
     the lock is held to percolate into the post in the true
     branch, to reason correctly about use of things guarded
     by the lock afterwards. We can refine this later if need be.
  */

  public static boolean holdsLock(Object obj) {
    if (InferUndefined.boolean_undefined()) {
      InferBuiltins.__set_locked_attribute(obj);
      return true;
    } else {
      return false;
    }
  }
}
