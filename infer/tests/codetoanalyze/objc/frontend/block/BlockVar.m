/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import "BlockVar.h"

@implementation BlockVar

+ (int)test {
    return 5;
}

+ (int)navigateToURLInBackground:(NSURL *)destination
                             resolver:(id)resolver {

    int (^addBlock)(int a, int b) = ^(int a, int b){
        NSError *error = nil;
        int res = [self test];
        return a + b + res;
    };
    return addBlock(1, 2);
}

@end
