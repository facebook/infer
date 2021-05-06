/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This file serves as a kind of template, used to separate the code needed
// to run the various testing tools (Klee, libfuzz, random fuzzer)
// from the code defining the blocks of the code under test.
// To use this file:
// 1) Include it,
// 2) Define the following values: NUM_THREADS, SCHEDULE_POINTS, and
//    CONTEXT_SWITCHES_BOUND.
// 3) Define a function int test_with_schedule(uint8_t*
// schedule, size_t len), which should create 'NUM_THREADS' base_frames and call
//     execute_schedule(
//          schedule,
//          len,
//          frames, // 'NUM_THREADS' base_frames.
//          NUM_THREADS,
//          CONTEXT_SWITCHES_BOUND
//      );

int test_with_schedule(uint8_t* schedule, size_t len);

#ifdef KLEE
#include <klee.h>

uint8_t* make_symbolic_schedule(int schedule_len, int max_id) {
  uint8_t* schedule = malloc(sizeof(int) * schedule_len);
  klee_make_symbolic(schedule, sizeof(int) * schedule_len, "schedule");
  for (int schedule_idx = 0; schedule_idx < schedule_len; ++schedule_idx) {
    uint8_t current_id = schedule[schedule_idx];
    klee_assume(current_id < max_id);
  }
  return schedule;
}

// Klee entry point.
int main(void) {
  // print_test();
  // return 0;
  uint8_t* schedule = make_symbolic_schedule(SCHEDULE_POINTS, NUM_THREADS);
  test_with_schedule(schedule, SCHEDULE_POINTS);
  return 0;
}
#endif

#ifdef RAND_FUZZ
static int fuzz_counter = 0;
int main(void) {
  uint8_t schedule[SCHEDULE_POINTS];
  while (true) {
    for (int idx = 0; idx < SCHEDULE_POINTS; ++idx) {
      schedule[idx] = rand() % NUM_THREADS;
    }
    printf("schedule %d: ", fuzz_counter);
    ++fuzz_counter;
    for (int idx = 0; idx < SCHEDULE_POINTS; ++idx) {
      printf("%hhu ", schedule[idx]);
    }
    printf("\n");
    test_with_schedule(schedule, SCHEDULE_POINTS);
  }
  return 0;
}
#endif // RAND_FUZZ

#ifdef ENUM_FUZZ
static int fuzz_counter = 0;
int main(void) {
  uint8_t schedule[SCHEDULE_POINTS];
  for (int idx = 0; idx < SCHEDULE_POINTS; ++idx) {
    schedule[idx] = 0;
  }
  while (true) {
    bool all_max = true;
    for (int idx = 0; idx < SCHEDULE_POINTS; ++idx) {
      if (schedule[idx] != NUM_THREADS - 1) {
        all_max = false;
        break;
      }
    }
    if (all_max) {
      return 0;
    }
    printf("schedule %d: ", fuzz_counter);
    ++fuzz_counter;
    for (int idx = 0; idx < SCHEDULE_POINTS; ++idx) {
      printf("%hhu ", schedule[idx]);
    }
    printf("\n");
    test_with_schedule(schedule, SCHEDULE_POINTS);
    for (int idx = 0; idx < SCHEDULE_POINTS; ++idx) {
      if (schedule[idx] < NUM_THREADS - 1) {
        ++schedule[idx];
        break;
      } else {
        for (int idx2 = idx; idx2 < SCHEDULE_POINTS; ++idx2) {
          if (schedule[idx2] == NUM_THREADS - 1) {
            schedule[idx2] = 0;
          } else {
            ++schedule[idx2];
            break;
          }
        }
        break;
      }
    }
  }
  return 0;
}
#endif // ENUM_FUZZ

#ifdef FUZZ
static int fuzz_counter = 0;
int LLVMFuzzerTestOneInput(uint8_t* schedule, size_t size) {
  if (size > SCHEDULE_POINTS) {
    return 0;
  }
  for (int idx = 0; idx < size; ++idx) {
    if (schedule[idx] >= NUM_THREADS) {
      return 0;
    }
  }
  printf("fuzz schedule: %d\n", fuzz_counter);
  ++fuzz_counter;
  // printf("schedule size: %zu\n", size);
  // for (int idx = 0; idx < size; ++idx) {
  //     printf("%hhu ", schedule[idx]);
  // }
  // printf("\n");
  test_with_schedule(schedule, size);
  return 0; // Non-zero return values are reserved for future use.
}
#endif // FUZZ
