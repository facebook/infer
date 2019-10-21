/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>
#import <Foundation/Foundation.h>

int example_function1() { return 111; } // change here

int example_function2() { return 2; }

namespace Shapes {

class Cube {
  int size;

 public:
  void set_size(int);
  int area() { return size * size * size; }; // change here
  void sort(Cube*, unsigned);
};

void Cube::set_size(int s) { size = s; }

void Cube::sort(Cube* xs, unsigned n) {
  std::sort(xs, xs + n, [](Cube a, Cube b) { return (a.area() < b.area()); });
}
} // namespace Shapes

@interface Person : NSObject

@property NSString* name;

- (BOOL)isEqual:(id)obj;

- (void)printName;

@end

@implementation Person

- (BOOL)isEqual:(id)other { // change here
  BOOL result = NO;
  if ([other isKindOfClass:[self class]]) {
    Person* otherObject = other;
    result = [self.name isEqualToString:[otherObject name]];
  }
  return result;
}

- (void)printName {
  NSLog(@"The person's name is %@", _name);
}

+ (void)multiply {
  double (^multiplyTwoValues)(double, double) =
      ^(double first, double second) { // change here
        return first * second;
      };

  double result = multiplyTwoValues(2, 4);

  NSLog(@"The result is %f", result);
}

@end

void sum() {
  double (^sumTwoValues)(double, double) =
      ^(double firstValue, double secondValue) {
        return firstValue + secondValue;
      };

  double result = sumTwoValues(2, 4);

  NSLog(@"The result is %f", result);
}
