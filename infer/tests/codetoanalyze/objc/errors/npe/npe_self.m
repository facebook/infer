/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>


@interface C : NSObject {
    int x;
    C* _currentCompositionState;
}
@property (nonatomic, copy, readonly) NSString *JSON;

@end

@implementation C

- (id)init {
    self = [super init];
    self->x = 10;
    return self;
}

- (void)captureManagerSessionDidStart
{
    __weak C *weakSelf = self;
    C *strongSelf = weakSelf;
    int x = strongSelf->x;
}

- (int)test
{
    if (_currentCompositionState != nil) {}
    return _currentCompositionState->x;
}

- (BOOL)isEqual:(id)object {
    if (object == self) return YES;
    if (![object isKindOfClass:[self class]]) return NO;
    C *other = (C *)object;
    return ([_JSON isEqualToString:other->_JSON]);
}

@end
