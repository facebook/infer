#import <Foundation/Foundation.h>

@interface User : NSObject

@end

@interface ContextManager : NSObject

- (instancetype)initWithLoggingHandler:(void (^)(void))loggingHandler;

- (id)foo:(void (^)(void))loggingHandler;

@end
@implementation User

- (ContextManager*)mutualContextManager_bad {
  return [[ContextManager alloc] initWithLoggingHandler:^{
    [self log]; // reports SELF_IN_BLOCK_PASSED_TO_INIT
    [self log]; // only one occurrence gets reported
  }];
}

- (ContextManager*)mutualContextManager_good1 {
  return [[ContextManager alloc] foo:^{
    [self log]; // no report because it's not passed to an initialiser
  }];
}

- (ContextManager*)mutualContextManager_good2 {
  __weak __typeof(self) weakSelf = self;
  return [[ContextManager alloc] initWithLoggingHandler:^{
    __strong __typeof(self) strongSelf = weakSelf;
    if (!strongSelf) {
      return;
    }
    [strongSelf log]; // no report because it's not self
  }];
}

- (void)log {
}

@end
