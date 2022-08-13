/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

public class InterproceduralInterfileUserInfo {
  static int getUserInfo(int k) {
    InterproceduralInterfileUser user = new InterproceduralInterfileUser("Martha");
    if (k == InterproceduralInterfileKind.PHONE) {
      return InterproceduralInterfileUserWrapper.getPhoneNumber(user);
    }
    return 0;
  }
}
