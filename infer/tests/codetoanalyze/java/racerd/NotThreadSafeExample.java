/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public class NotThreadSafeExample{

  Integer f;

  public void tsBad() { /*Shouldn't report*/
    f = 24;
  }

}
