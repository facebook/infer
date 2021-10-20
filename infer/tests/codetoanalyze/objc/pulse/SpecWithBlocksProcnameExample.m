/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface UploadJob : NSObject

@end

typedef void (^ContentSourceLocalMatchHandler)();
typedef void (^ContentSourceExternalMatchHandler)();

typedef NS_ENUM(NSUInteger, ContentSourceSubtypes) {
  ContentSourceSubtypesLocal,
  ContentSourceSubtypesExternal,
};

@interface ContentSource

- (void)matchLocal:(ContentSourceLocalMatchHandler)localMatchHandler
          external:(ContentSourceExternalMatchHandler)externalMatchHandler;

@end

@implementation ContentSource {
  ContentSourceSubtypes _subtype;
}

- (void)matchLocal:(ContentSourceLocalMatchHandler)localMatchHandler
          external:(ContentSourceExternalMatchHandler)externalMatchHandler {
  switch (_subtype) {
    case ContentSourceSubtypesLocal: {
      if (localMatchHandler) {
        localMatchHandler();
      }
      break;
    }
    case ContentSourceSubtypesExternal: {
      externalMatchHandler();
      break;
    }
  }
}

@end

@interface SpecWithBlocksProcnameExample

@end

@implementation SpecWithBlocksProcnameExample

- (UploadJob*)nilBlockCallBadLatent:(ContentSource*)contentSource {
  __block UploadJob* job = nil;
  [contentSource
      matchLocal:^() {
        job = [[UploadJob alloc] init];
      }
        external:NULL];
  return job;
}

@end
