/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void dispatch_async(dispatch_queue_t queue, dispatch_block_t block) { block(); }

void dispatch_after(dispatch_time_t when,
                    dispatch_queue_t queue,
                    dispatch_block_t block) {
  block();
}

void dispatch_group_async(dispatch_group_t group,

                          dispatch_queue_t queue,
                          dispatch_block_t block) {
  block();
}

long dispatch_group_wait(dispatch_group_t group, dispatch_time_t timeout) {
  block();
}

void dispatch_barrier_async(dispatch_queue_t queue, dispatch_block_t block) {
  block();
}

void dispatch_source_set_cancel_handler(dispatch_source_t source,
                                        dispatch_block_t handler) {
  block();
}

void dispatch_source_set_event_handler(dispatch_source_t source,
                                       dispatch_block_t handler) {
  block();
}

void dispatch_group_notify(dispatch_group_t group,
                           dispatch_queue_t queue,
                           dispatch_block_t block) {
  block();
}
