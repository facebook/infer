/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
