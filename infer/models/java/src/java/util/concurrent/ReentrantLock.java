/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* Reentrant Lock model
 * This is a partial model. Several methods such as getowner(),  hasQueueThread(),
 * getQueueLength() are not modelled.
 * A reentrant lock is one where if you try to lock() a lock already held by
 * the current thread, you succeed and continue. In a non-reentrant lock you could deadlock.
 * This is not reflected.  java.util.concurrent ReentrantLocks have some fairness
 * guarantees and some things about scheduling
 * when the lock is held by another thread, not considered here..
 *
 */

package java.util.concurrent.locks;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.util.concurrent.TimeUnit;

public abstract class ReentrantLock extends Lock implements java.io.Serializable {

  /**
   * We should be able to delete this and inherit from the modle in Lock.java, when improved
   * treatment of dynamic dispatch lands
   */
  public void lock() {
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
   * the current thread. This model does not consider that possibility. We should be able to delete
   * this and inherit from the modle in Lock.java, when improved treatment of dynamic dispatch lands
   */
  public void unlock() {
    InferBuiltins.__delete_locked_attribute(this);
  }

  /** TODO. This requires us to abduce lockedness. Do later. public boolean isLocked() { */
}
