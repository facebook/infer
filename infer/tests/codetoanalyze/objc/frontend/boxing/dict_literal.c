/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */



#import <Foundation/Foundation.h>

NSDictionary* get_array1() {
    
    return  [NSDictionary dictionaryWithObjectsAndKeys:
             @"Matt", @"firstName", @"Galloway", @"lastName",
             @28, @"age", nil];
}

NSDictionary* get_array2() {
    
    return @{@"firstName" : @"Matt", @"lastName" : @"Galloway", @"age" : @28};
}

