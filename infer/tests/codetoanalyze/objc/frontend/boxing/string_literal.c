/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>

NSString* get_string1() {

    return  [NSString stringWithUTF8String: "Hello World!" ];
}

NSString* get_string2() {

    return @"Hello World!";
}
