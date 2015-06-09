/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject

@property int x;

+ (instancetype)sharedInstance;

@end

@implementation A {
}

+ (instancetype)sharedInstance
{
    static dispatch_once_t once;
    static id sharedInstance;
    dispatch_once(&once,
                  ^{
                      sharedInstance = [[self alloc] init];
                  });
    
    return sharedInstance;
}


+ (instancetype)trans_SI
{
    static id sharedInstance;
 
     void (^dummy_block)()=^{
                      sharedInstance = [[self alloc] init];
                  };
    dummy_block();
    return sharedInstance;

}
@end

int main () {
    A *b =[A sharedInstance];
    b.x=17;
    return 0;
}






