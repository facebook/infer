/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

class InterprocedurlaInterfileTaint {
  void taintedUserToLogBad() {
    InterproceduralInterfileLoggerWrapper.log(
        InterproceduralInterfileUserInfo.getUserInfo(InterproceduralInterfileKind.PHONE));
  }
}
