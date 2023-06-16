/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "Message.h"

void log_message_good() { NSLog(message_to_me()); }

void log_data_bad() { NSLog(important_data()); }

void log_message_from_block_good() {
  process_message(^(NSString* message) {
    NSLog(message);
  });
}

void log_message_from_block_bad() {
  process_data(^(NSString* message) {
    NSLog(message);
  });
}

void log_important_news_good() { NSLog([ImportantNews produce_news]); }

void log_important_global_bad() { NSLog([GlobalNews produce_news]); }

void log_news_field_good() {
  ImportantNews* my_news = [ImportantNews new];
  NSLog(my_news->news);
}

void log_presenter_field_bad() {
  GlobalNews* my_news = [GlobalNews new];
  NSLog(my_news->presenter);
}
