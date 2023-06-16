/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

NSString* message_to_me();

NSString* important_data();

typedef void (^MessageBlock)(NSString* message);

void process_message(MessageBlock completion);

void process_data(MessageBlock completion);

@interface ImportantNews : NSObject {

 @public
  NSString* news;
}

+ (NSString*)produce_news;

@end

@interface GlobalNews : NSObject {
 @public
  NSString* presenter;
}

+ (NSString*)produce_news;

@end
