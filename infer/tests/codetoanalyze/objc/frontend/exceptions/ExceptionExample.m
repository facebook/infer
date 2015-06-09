/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>

@interface ExceptionExample : NSObject

@end


@implementation ExceptionExample

-(void) test {
    @try {
        NSString *s = [NSString alloc];
    }
    @catch (NSException *exception) {
    }
    @finally {
        [self  description];
    }
}

-(void) test1 {
    NSString *s = [NSString alloc];
    if (s) {
        @throw [NSException exceptionWithName:@"Something is not right exception"
                                       reason:@"Can't perform this operation because of this or that"
                                     userInfo:nil];
    }

}

@end
