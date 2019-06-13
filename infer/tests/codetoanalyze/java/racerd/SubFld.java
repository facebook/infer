/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import javax.annotation.concurrent.ThreadSafe;

// Fields must encapsulate the class they are declared in, not
// the class they are potentially inherited into.

@ThreadSafe
class SuperFld {

  private int f = 0;

  public int getF() {
    return f; // should *not* report read/write race with SubFld.setF()
  }

  protected int g = 0;

  public int getG() {
    return g; // must report read/write race with SubFld.setG()
  }
}

@ThreadSafe
public class SubFld extends SuperFld {

  private int f = 0;

  public synchronized void setF() {
    f = 5; // should *not* report
  }

  public synchronized void setG() {
    g = 5; // must report
  }
}
