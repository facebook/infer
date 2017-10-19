/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
class Alias{
  A a,b;

  public void foo() {
    int x;
    synchronized(this){
      a = b;
      a.f = 101;
    }
    x = b.f;  // may_alias
  }

  public void bar(A a,A b) {
     int x;
     synchronized(this){
       a.f = 101;
     }
    x = b.f;  // no may_alias needed, argument treatment suffices
  }
}

class A { int f = 0; }
