/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.concurrent.locks;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.util.concurrent.TimeUnit;

/*
Lock is in actuality an interface, but is implemented here as an abstract class for
modelling reasons. Note that we don't provide a model here for the newCondition() method.
*/

public abstract class Lock {

  void lock() {
    InferBuiltins.__set_locked_attribute(this);
  }

  /** Sometimes doesn't get a lock. */
  public void lockInterruptibly() throws InterruptedException {
    if (InferUndefined.boolean_undefined()) {
      InferBuiltins.__set_locked_attribute(this);
    } else {
      throw new InterruptedException();
    }
  }

  /** Again, doesn't always succeed */
  public boolean tryLock() {
    if (InferUndefined.boolean_undefined()) {
      InferBuiltins.__set_locked_attribute(this);
      return true;
    } else {
      return false;
    }
  }

  public boolean tryLock(long timeout, TimeUnit unit) throws InterruptedException {
    if (InferUndefined.boolean_undefined()) {
      throw new InterruptedException();
    }

    if (InferUndefined.boolean_undefined()) {
      InferBuiltins.__set_locked_attribute(this);
      return true;
    } else {
      return false;
    }
  }

  /**
   * In some implementations (like ReentrantLock) an exception is thrown if the lock is not held by
   * the current thread. This model does not consider that possibility.
   */
  public void unlock() {
    InferBuiltins.__delete_locked_attribute(this);
  }
}
