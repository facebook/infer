/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef SCHEDULER_H
#define SCHEDULER_H

#include <stdbool.h>

typedef struct {
  int pc;
  int derived_frame_type;
  void* derived_frame;
} base_frame;

#define PC_DONE -1
#define PC_ABORT -2

/** Steps the thread with the current frame and tests for correctness.
 */
extern int step(base_frame* frame);

base_frame* make_frame(void* derived_frame, int derived_frame_type);
void init_frame(base_frame* frame, void* derived_frame, int derived_frame_type);
uint8_t* make_symbolic_schedule(int schedule_len, int max_id);
int execute_schedule(uint8_t* schedule, size_t schedule_len, base_frame* frames,
                     int num_frames, int max_context_switches);

#endif
