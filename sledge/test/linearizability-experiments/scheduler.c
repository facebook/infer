/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "scheduler.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

base_frame* make_frame(void* derived_frame, int derived_frame_type) {
  base_frame* frame = malloc(sizeof(base_frame));
  init_frame(frame, derived_frame, derived_frame_type);
  return frame;
}

void init_frame(base_frame* frame, void* derived_frame,
                int derived_frame_type) {
  frame->pc = 0;
  frame->derived_frame = derived_frame;
  frame->derived_frame_type = derived_frame_type;
}

int execute_schedule(uint8_t* schedule, size_t schedule_len, base_frame* frames,
                     int num_frames, int max_context_switches) {
  int num_context_switches = 0;
  int num_done = 0;
  uint8_t previous_op = schedule[0];
  uint8_t current_op = previous_op;
  assert(schedule != NULL);
  bool frame_done = false; /* Tracks whether the last step ended a frame. */
  for (size_t schedule_idx = 0;
       schedule_idx < schedule_len && num_done < num_frames; ++schedule_idx) {
    current_op = schedule[schedule_idx];
    if (current_op != previous_op && !frame_done) {
      ++num_context_switches;
      if (num_context_switches > max_context_switches) {
        return num_context_switches;
      }
    }
    previous_op = current_op;

    base_frame* frame = &frames[current_op];
    if (frame->pc == PC_DONE) {
      continue;
    } else {
      frame_done = false;
      int next_pc = step(frame);
      frame->pc = next_pc;
      if (next_pc == PC_DONE) {
        frame_done = true;
        ++num_done;
      } else if (next_pc == PC_ABORT) {
        break;
      }
    }
  }
  return num_context_switches;
}
