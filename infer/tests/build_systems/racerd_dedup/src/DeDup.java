/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package build_systems.threadsafety;

import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
class DeDup {

  Integer field;
  Integer fielda, fieldb;

  /* Only want one rather than two reports */
  void two_fields() {
    foo();
  }

  private void foo() {
    fielda = 88;
    fieldb = 99;
  }

  /*Only the first write should be reported*/
  void two_writes() {
    field = 22;
    field = 84;
  }

  /*Only the first read should be reported*/
  void two_reads() { // parallel reads are OK
    Integer local;
    local = field;
    local = field + 1;
  }

  /*Both accesses should be reported*/
  void write_read() { // parallel reads are OK
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

  Integer colocated_read, colocated_write;

  /*Should  only report colocated write, not read, from readandwrite()*/
  void colocated_read_write() {
    read_and_write();
  }

  /*Should report*/
  void separate_write_to_colocated_read() {
    colocated_read = 88;
  }

  private void read_and_write() {
    Integer x = colocated_read;
    colocated_write = 99;
  }
}
