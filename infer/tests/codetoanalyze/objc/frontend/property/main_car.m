/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import "Car.h"

int main() {
    Car *honda = [[Car alloc] init];
    honda.running = YES;                
    NSLog(@"%d", honda.running);        
  return 0;
}
