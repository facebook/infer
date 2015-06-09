/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>

NSArray* get_array() {
  NSArray *animals = [NSArray arrayWithObjects:@"cat", @"dog", nil];
  return @[@"cat", @"dog"];
}
