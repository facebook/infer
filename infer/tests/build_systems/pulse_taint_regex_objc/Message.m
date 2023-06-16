/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "Message.h"

NSString* message_to_me() { return @"hello"; }

NSString* important_data() { return @"my_data"; }

void process_message(MessageBlock completion) { completion(@"my_message"); }

void process_data(MessageBlock completion) { completion(@"my_data"); }

@implementation ImportantNews

+ (NSString*)produce_news {
  return @"the news";
}

@end

@implementation GlobalNews

+ (NSString*)produce_news {
  return @"the news";
}

@end
