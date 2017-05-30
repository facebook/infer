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
  native static boolean isMainThread();
  static void assertMainThread(){}
  static void assertHoldsLock(Object lock){}
}

class OurThreadUtil{ /*This is like AndroidThreadUtil*/
  native static boolean isUiThread();
  static void assertOnUiThread(){}
}


@ThreadSafe
class RaceWithMainThread{

  Integer f;

  void main_thread_OK(){
      OurThreadUtils.assertMainThread();
      f = 88;
   }

  Integer f1;

  void main_thread1_OK(){
      OurThreadUtil.assertOnUiThread();
      f1 = 88;
   }


   void main_thread_indirect_OK() {
     main_thread_OK();
     f = 77;
   }

  void read_from_main_thread_OK(){
    Integer x;
    OurThreadUtils.assertMainThread();
    x = f;
  }

  void read_unprotected_unthreaded_Bad(){
    Integer x;
    x = f;
  }

  void read_unprotected_unthreaded1_Bad(){
    Integer x;
    x = f1;
  }

  /*There is a particularly subtle idiom which avoids races, where a
    variable can be read without protection on the main thread, if
    it is written with protection on the main thread and read with
    protection off. The next three methods do this safely, and the fourth
    unsafely.
  */
  Integer i;

  void protected_write_on_main_thread_OK() {
    OurThreadUtils.assertMainThread();
    synchronized (this) {
      i = 99;
    }
  }

  void unprotected_read_on_main_thread_OK() {
    Integer x;
    OurThreadUtils.assertMainThread();
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
      OurThreadUtils.assertHoldsLock(this);
      g = 88;
   }

   void holds_lock_indirect_OK() {
     holds_lock_OK();
     g = 77;
   }

Integer ff;

 void conditional_Ok(boolean b){
   if (b)
   { /*People not literally putting this assert inside if's,
       but implicitly by method calls */
     OurThreadUtils.assertMainThread();
     ff = 88;
   }
 }



 void conditional_Bad(boolean b){
   if (b)
   {
     OurThreadUtils.assertMainThread();
     ff = 88;
   } else {
     ff = 99;
   }
 }

 void conditional_isMainThread_Ok(){
   if (OurThreadUtils.isMainThread())
   {
     ff = 88;
   }
 }

 void conditional_isUiThread_Ok(){
   if (OurThreadUtil.isUiThread())
   {
     ff = 88;
   }
 }


 void conditional_isMainThread_ElseBranch_Bad(){
   if (OurThreadUtils.isMainThread())
   {
    synchronized(this){
     ff = 88;
   }
   } else {
     ff = 99;
   }
 }

 void conditional_isUiThread_ElseBranch_Bad(){
   if (OurThreadUtil.isUiThread())
   {
    synchronized(this){
     ff = 88;
   }
   } else {
     ff = 99;
   }
 }


 void conditional_isMainThread_Negation_Bad(){
   if (!OurThreadUtils.isMainThread())
   {
     ff = 88;
   }
 }

 void conditional_isMainThread_ElseBranch_Ok(){
   if (!OurThreadUtils.isMainThread())
   {
    synchronized(this){
     ff = 88;
   }
   } else {
     ff = 99;
   }
 }

}
