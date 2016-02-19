/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

void __set_observer_attribute(id);
void __set_unsubscribed_observer_attribute(id);

@interface NSNotificationCenter : NSObject

- (void)addObserver:(id)notificationObserver
           selector:(SEL)notificationSelector
               name:(NSString*)notificationName
             object:(id)notificationSender;

- (void)removeObserver:(id)notificationObserver;

- (void)removeObserver:(id)notificationObserver
                  name:(NSString*)notificationName
                object:(id)notificationSender;
@end

@implementation NSNotificationCenter

- (void)addObserver:(id)notificationObserver
           selector:(SEL)notificationSelector
               name:(NSString*)notificationName
             object:(id)notificationSender {
  __set_observer_attribute(notificationObserver);
}

- (void)removeObserver:(id)notificationObserver {
  __set_unsubscribed_observer_attribute(notificationObserver);
}

- (void)removeObserver:(id)notificationObserver
                  name:(NSString*)notificationName
                object:(id)notificationSender {
  __set_unsubscribed_observer_attribute(notificationObserver);
}

@end
