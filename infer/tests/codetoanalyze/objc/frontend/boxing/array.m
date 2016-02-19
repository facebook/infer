/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

int main() {

  NSString* s;

  NSArray* germanCars = @[
    @"Mercedes-Benz",
    @"BMW",
    @"Porsche",
    @"Opel",
    @"Volkswagen",
    @"Audi"
  ];
  s = germanCars[3];

  for (NSString* item in germanCars) {
    NSLog(@"%@", item);
  }

  return 0;
}
