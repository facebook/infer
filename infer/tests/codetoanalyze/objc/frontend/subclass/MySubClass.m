/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import "MySubClass.h"

@implementation MySubclass : MyClass {
}

- (int)myNumber {
  
int subclassNumber = [super myNumber] + 1;
  return subclassNumber;
}

@end
