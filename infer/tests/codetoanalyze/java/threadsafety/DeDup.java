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
class DeDup{

Integer field;

  /*Only the first write should be reported*/
  void two_writes(){
   field = 22;
   field = 84;
   }

  /*Only the first read should be reported*/
  void two_reads(){ //parallel reads are OK
   Integer local;
   local = field;
   local = field+1;
  }

  /*Both accesses should be reported*/
  void write_read(){ //parallel reads are OK
   Integer local;
   field = 87;
   local = field;
  }

  /*Should only report the first write, which happens to be interprocedural*/
  void twoWritesOneInCaller() {
    writeToField();
    field = 22;
  }

  /*Should not report directly as provate method*/
  private void writeToField() {
    field = 33;
  }

}
