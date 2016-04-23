/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;

public abstract class ReentrantLock extends Lock implements java.io.Serializable {

    /**
    * We should be able to delete this and inherit from the modle in Lock.java,
    * when improved treatment of dynamic dispatch lands
    */
    public void lock() {
      InferBuiltins.__set_locked_attribute(this);
    }

     /**
     * In some implementations (like ReentrantLock) an exception is thrown if the lock
     * is not held by the current thread. This model does not consider that possibility.
     * We should be able to delete this and inherit from the modle in Lock.java,
     * when improved treatment of dynamic dispatch lands
     */
    public void unlock() {
      InferBuiltins.__delete_locked_attribute(this);
    }

     /**
      *  TODO. This requires us to abduce lockedness. Do later.
     public boolean isLocked() {
     */
}
