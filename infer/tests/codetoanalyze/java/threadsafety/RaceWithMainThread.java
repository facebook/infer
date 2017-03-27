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

  void read_from_main_thread_OK(){
    Integer x;
    o.assertMainThread();
    x = f;
  }

  void read_unprotected_unthreaded_Bad(){
    Integer x;
    x = f;
  }

  /*There is a particularly subtle idiom which avoids races, where a
    variable can be read without protection on the main thread, if
    it is written with protection on the main thread and read with
    protection off. The next three methods do this safely, and the fourth
    unsafely.
  */
  Integer i;

  void protected_write_on_main_thread_OK() {
    o.assertMainThread();
    synchronized (this) {
      i = 99;
    }
  }

  void unprotected_read_on_main_thread_OK() {
    Integer x;
    o.assertMainThread();
    x = i;
  }

  void protected_read_off_main_thread_OK() {
    Integer x;
    synchronized (this) {
      x = i;
    }
  }

  void readProtectedUnthreadedBad(){
    Integer x;
    synchronized (this){
      x = f;
    }
  }

  Integer g;

  void holds_lock_OK(){
      o.assertHoldsLock(this);
      g = 88;
   }

   void holds_lock_indirect_OK() {
     holds_lock_OK();
     g = 77;
   }
}
