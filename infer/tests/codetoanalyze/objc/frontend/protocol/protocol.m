/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import<Foundation/NSObject.h>

@protocol Foo
- (void)fooMethod;

  @property (retain) NSString *foo;
@end


@interface Bla: NSObject
- (void)fooMethod;
@end

@implementation Bla: NSObject

- (void)fooMethod {
  if ([self conformsToProtocol:@protocol(Foo)]) {
    return;
  }
}
@end
