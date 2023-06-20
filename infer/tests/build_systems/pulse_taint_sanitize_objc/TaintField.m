/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "Account.h"

void logAccount_bad() {
  Account* account = [[Account alloc] init];
  NSLog(@"%@", account->accountID);
}
