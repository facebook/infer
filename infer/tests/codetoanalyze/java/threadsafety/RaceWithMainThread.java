/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

class OurThreadUtils{
  void assertMainThread(){}
  void assertHoldsLock(Object lock){}
}

@ThreadSafe
class RaceWithMainThread{

  Integer f;
  OurThreadUtils o;

  void main_thread_OK(){
      o.assertMainThread();
      f = 88;
   }

   void main_thread_indirect_OK() {
     main_thread_OK();
     f = 77;
   }

  void holds_lock_OK(){
      o.assertHoldsLock(this);
      f = 88;
   }

   void holds_lock_indirect_OK() {
     holds_lock_OK();
     f = 77;
   }
}
