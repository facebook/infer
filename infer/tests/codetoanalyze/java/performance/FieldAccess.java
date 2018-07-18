/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;



public class FieldAccess{

  public class Test{
  int a;
}

  void iterate_upto_field_size(Test test){
     for (int ci = 0; ci < test.a; ++ci) {
    }
  }


}
