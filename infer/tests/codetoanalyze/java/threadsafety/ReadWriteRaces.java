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

@ThreadSafe
class ReadWriteRaces{

Integer safe_read;
Integer racy;

  void m0_OK(){
   Integer local;
   local = safe_read;
   }

  void m0_OK2(){ //parallel reads are OK
   Integer local;
   local = safe_read;
  }

  void m1(){ // A read where there are other writes
   Integer local;
   local = racy;
  }

  public void m2(){
   racy = 88;
  }

  public void m3(){
   racy = 99;
  }

  // need to report races involving safe writes in order to get this one
  public synchronized void FN_syncWrite1() {
    racy = 42;
  }

  public void FN_syncWrite2() {
    synchronized(this) {
      racy = 43;
    }
  }

  private synchronized void syncWrite3() {
    racy = 44;
  }

  public void FN_callSyncWrite3() {
    syncWrite3();
  }


}
