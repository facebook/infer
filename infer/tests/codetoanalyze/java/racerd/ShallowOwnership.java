/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */


import com.facebook.infer.annotation.ThreadSafe;

@ThreadSafe
class ShallowOwnership {
  ShallowOwnership next;
  static ShallowOwnership global;

  void globalNotOwnedBad() {
    global.next = null;
  }

  void reassignBaseToGlobalBad(){
    ShallowOwnership x = new ShallowOwnership();
    x = global;
    x.next = null;
  }

  void reassignPathToGlobalBad() {
    ShallowOwnership x = new ShallowOwnership();
    x.next = global;
    x.next.next = null;
  }

}
