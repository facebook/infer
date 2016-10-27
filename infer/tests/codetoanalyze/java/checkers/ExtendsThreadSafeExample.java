/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
class ExtendsThreadSafeExample extends ThreadSafeExample{

  Integer field;

  (* Presently,we will warn not just on overwridden methods from
  @ThreadSafe class, but potentially on other methods in subclass *)
  public void newmethodBad() {
     field = 22;
  }

  (* Bad now that it's overridden *)
  public void tsOK() {
     field = 44;
  }

}
