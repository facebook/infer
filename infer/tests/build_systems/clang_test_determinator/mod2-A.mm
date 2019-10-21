/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>
#import <Foundation/Foundation.h>

int example_function1() { return 1; }

int example_function2() { return 222; } // change here

namespace Shapes {

class Cube {
  int size;

 public:
  void set_size(int);
  int area() { return size * size; };
  void sort(Cube*, unsigned);
};

void Cube::set_size(int s) { size = s; }

void Cube::sort(Cube* xs, unsigned n) {
  // this is a lambda folks //change here
  std::sort(xs, xs + n, [](Cube a, Cube b) { return (a.area() < b.area()); });
}
} // namespace Shapes

@interface Person : NSObject

@property NSString* name;

- (BOOL)isEqual:(id)obj;

- (void)printName;

@end

@implementation Person

- (BOOL)isEqual:(id)obj {
  BOOL result = NO;
  if ([obj isKindOfClass:[self class]]) {
    Person* otherObject = obj;
    result = [self.name isEqualToString:[otherObject name]];
  }
  return result;
}

- (void)printName {
  NSLog(@"The name is %@", _name); // change here
}

+ (void)multiply {
  double (^multiplyTwoValues)(double, double) =
      ^(double firstValue, double secondValue) {
        return firstValue * secondValue;
      };

  double result = multiplyTwoValues(2, 4);

  NSLog(@"The result is %f", result);
}

@end

void sum() {
  double (^sumTwoValues)(double, double) =
      ^(double first, double second) { // change here
        return first + second;
      };

  double result = sumTwoValues(2, 4);

  NSLog(@"The result is %f", result);
}
