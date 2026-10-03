/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface Format
+ (id)format:(const char*)format, ...;
@end

@interface Logger
+ (void)log:(id)message;
@end

extern int log_level;

long now(void);

#define LOG(fmt, ...)                                  \
  do {                                                 \
    if (log_level > 0)                                 \
      [Logger log:[Format format:fmt, ##__VA_ARGS__]]; \
  } while (0)

void log_elapsed_time_ok(void) {
  long start = now();
  long end = now();
  LOG("elapsed: %ld", end - start);
}
