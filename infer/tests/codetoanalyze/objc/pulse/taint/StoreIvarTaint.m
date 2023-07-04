/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface Item : NSObject
@property(nonatomic, copy) NSString* token;
@end

typedef void (^LibraryFetchCompletionBlock)(NSArray<id>* items,
                                            NSArray<NSError*>* errors);

@interface Library : NSObject
+ (instancetype)sharedInstance;
@end

@implementation Library

+ (instancetype)sharedInstance {
  static dispatch_once_t dispatchOnceToken;
  static Library* sharedLibrary = nil;
  dispatch_once(&dispatchOnceToken, ^{
    sharedLibrary = [Library new];
  });
  return sharedLibrary;
}

- (void)fetchWithCompletion:(LibraryFetchCompletionBlock)completion {
}

@end

@interface StoreIvarTaint : NSObject

@end

@implementation StoreIvarTaint {
  NSString* _token;
}

- (void)_fetchToken_bad {
  __weak __typeof(self) weakSelf = self;
  [[Library sharedInstance]
      fetchWithCompletion:^(NSArray<Item*>* accounts,
                            NSArray<NSError*>* _Nonnull errors) {
        NSString* const accessToken = [accounts firstObject].token;
        NSString* token = [accessToken copy];
        [self setToken:token];
      }];
}

- (void)setToken:(NSString*)token {
  _token = token;
}

@end

@interface OtherClass : NSObject
- (NSString*)getSource;
@end

@implementation OtherClass {
  NSString* _item;
 @public
  NSString* _name;
}

- (void)setItem:(NSString*)item {
  _item = item;
}

- (NSString*)getItem {
  return _item;
}

@end

void storeIvarBad() {
  OtherClass* c = [OtherClass new];
  NSString* source = [c getSource];
  [c setItem:source];
}

void storeIvarGood() {
  OtherClass* c = [OtherClass new];
  NSString* source = [c getSource];
  c->_name = source;
}
