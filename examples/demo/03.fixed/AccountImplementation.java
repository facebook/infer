/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class AccountImplementation implements Account {

  int mBalance = 0;

  private void addToBalance(int amount) {
    mBalance += amount;
  }

  public synchronized void deposit(int amount) {
    if (amount > 0) {
      addToBalance(amount);
    }
  }

  public synchronized int withdraw(int amount) {
    if (amount >= 0 && mBalance - amount >= 0) {
      addToBalance(-amount);
      return mBalance;
    } else {
      return 0;
    }
  }
}
