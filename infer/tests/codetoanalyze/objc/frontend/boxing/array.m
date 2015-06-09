/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import<Foundation/Foundation.h>



int main() {

  NSString *s;

  NSArray *germanCars = @[@"Mercedes-Benz", @"BMW", @"Porsche",
                            @"Opel", @"Volkswagen", @"Audi"];
  s = germanCars[3];

  for (NSString *item in germanCars) {
    NSLog(@"%@", item);
  }

  return 0;

}
