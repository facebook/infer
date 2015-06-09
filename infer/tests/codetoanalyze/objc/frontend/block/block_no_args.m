/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

int g;

@interface My_manager :NSObject
- (int) my_mehtod;

@end

@implementation My_manager

- (int) my_mehtod
{
    g=7;
    void (^b)();
    int z=3;
    b=^( ){
        g=z+3;
    };
    b();
    return z;
}



@end
