/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Test {

  public void select() {
    String a = "SELeCT id FROM ";
    String b = "table";
    String c = " WHERE something";
    String d = a + b + c;
  }

  public void insert() {
    int id = 10;
    String a = "INSERT into table VALUES (1) WHERE id=";
    String q = a + Integer.toString(id);
  }

}
