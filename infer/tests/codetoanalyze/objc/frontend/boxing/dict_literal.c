/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

NSDictionary* get_array1() {

  return [NSDictionary dictionaryWithObjectsAndKeys:@"Matt",
                                                    @"firstName",
                                                    @"Galloway",
                                                    @"lastName",
                                                    @28,
                                                    @"age",
                                                    nil];
}

NSDictionary* get_array2() {

  return @{@"firstName" : @"Matt", @"lastName" : @"Galloway", @"age" : @28};
}
