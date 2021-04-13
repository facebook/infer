/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Testing ProducerConsumerQueue from
// https://github.com/facebook/folly/blob/master/folly/ProducerConsumerQueue.h

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ref_queue.h"
#include "scheduler.h"

#define NUM_THREADS 2
#define CONTEXT_SWITCHES_BOUND 2
#define SCHEDULE_POINTS 8
#include "test_mains.h"

// size must be >= 2.
#define QUEUE_DEPTH 2

#define READ_BUG false
#define SIZE_BUG true

typedef enum { CONSUMER, PRODUCER } frame_type;

typedef struct {
  int size;
  int readIndex;
  int writeIndex;
  data_type* records;
} queue;

typedef struct {
  queue* q;
  ref_queue* ref;
  data_type v;
  int currentWrite;
  int nextRecord;
} producer_frame;

typedef struct {
  queue* q;
  ref_queue* ref;
  int currentRead;
  int currentWrite; // For the bug
  int nextRecord;
  data_type result;
} consumer_frame;

void init_queue(queue* q, size_t size) {
  q->size = size;
  q->readIndex = 0;
  q->writeIndex = 0;
  q->records = malloc(sizeof(data_type) * size);
}

consumer_frame* make_consumer_frame(queue* q, ref_queue* ref) {
  consumer_frame* frame = malloc(sizeof(consumer_frame));
  frame->q = q;
  frame->ref = ref;
  return frame;
}

producer_frame* make_producer_frame(queue* q, ref_queue* ref) {
  producer_frame* frame = malloc(sizeof(producer_frame));
  frame->q = q;
  frame->ref = ref;
  return frame;
}

data_type val = 0;

// write
int step_producer(producer_frame* frame, int pc) {
  queue* q = frame->q;
  switch (pc) {
  case 0:
    frame->v = val;
    val++;
    frame->currentWrite = q->writeIndex;
    frame->nextRecord = frame->currentWrite + 1;
    if (frame->nextRecord == q->size) {
      frame->nextRecord = 0;
    }
    pc = 1;
    break;

  case 1:
    if (frame->nextRecord != q->readIndex) {
      pc = 2;
    } else {
      bool queue_result = false;
      bool ref_result = ref_enqueue(frame->ref, frame->v);
      if (ref_result != queue_result) {
        printf("ref_result: %d, queue_result: %d\n", ref_result, queue_result);
      }
      assert(ref_result == queue_result);
      pc = 0;
    }
    break;

  case 2:
    q->records[frame->currentWrite] = frame->v;
    pc = 3;
    break;

  case 3:
    q->writeIndex = frame->nextRecord;
    bool queue_result = true;
    // printf("test enqueue: %d\n", frame->v);
    bool ref_result = ref_enqueue(frame->ref, frame->v);
    assert(ref_result == queue_result);
    pc = 0;
    break;

  default:
    assert(false);
  }
  return pc;
}

// read
int step_consumer(consumer_frame* frame, int pc) {
  queue* q = frame->q;
  switch (pc) {
  case 0:
    frame->currentRead = q->readIndex;
    pc = 1;
    break;

  case 1:
    if (frame->currentRead == q->writeIndex) {
      // queue is empty
      data_type ref_result = ref_dequeue(frame->ref);
      assert(ref_result == EMPTY);
      pc = 0;
      break;
    }
    frame->nextRecord = frame->currentRead + 1;
    if (frame->nextRecord == q->size) {
      frame->nextRecord = 0;
    }
    pc = 2;
    break;

  case 2:
    frame->result = q->records[frame->currentRead];
    pc = 3;
    break;

  case 3:
    q->readIndex = frame->nextRecord;
    data_type ref_result = ref_dequeue(frame->ref);
    assert(ref_result == frame->result);
    pc = 0;
    break;

  default:
    assert(false);
  }
  return pc;
}

int step_consumer_bug(consumer_frame* frame, int pc) {
  queue* q = frame->q;
  switch (pc) {
  case 0:
    frame->currentWrite = q->writeIndex;
    pc = 1;
    break;

  case 1:
    frame->currentRead = q->readIndex;
    if (frame->currentRead == frame->currentWrite) {
      // queue is empty
      data_type ref_result = ref_dequeue(frame->ref);
      assert(ref_result == EMPTY);
      pc = 0;
      break;
    }
    frame->nextRecord = frame->currentRead + 1;
    if (frame->nextRecord == q->size) {
      frame->nextRecord = 0;
    }
    pc = 2;
    break;

  case 2:
    frame->result = q->records[frame->currentRead];
    pc = 3;
    break;

  case 3:
    q->readIndex = frame->nextRecord;
    data_type ref_result = ref_dequeue(frame->ref);
    assert(ref_result == frame->result);
    pc = 0;
    break;

  default:
    assert(false);
  }
  return pc;
}

// Runs the operation associated with the given frame until reaching the next
// block. That is, the next statement accessing shared memory.
int step(base_frame* frame) {
  assert(frame != NULL);
  if (frame->derived_frame_type == CONSUMER) {
    consumer_frame* derived_frame = (consumer_frame*)frame->derived_frame;
    assert(derived_frame != NULL);
    if (READ_BUG) {
      return step_consumer_bug(derived_frame, frame->pc);
    } else {
      return step_consumer(derived_frame, frame->pc);
    }
  } else if (frame->derived_frame_type == PRODUCER) {
    producer_frame* derived_frame = (producer_frame*)frame->derived_frame;
    assert(derived_frame != NULL);
    return step_producer(derived_frame, frame->pc);
  } else {
    assert(false);
  }
}

int test_with_schedule(uint8_t* schedule, size_t len) {
  queue test_queue;

  init_queue(&test_queue, QUEUE_DEPTH);
  // From the folly documentation:
  // "Also, note that the number of usable slots in the queue at any
  // given time is actually (size-1)..."
  ref_queue* ref;
  if (SIZE_BUG) {
    ref = new_ref_queue(QUEUE_DEPTH);
  } else {
    ref = new_ref_queue(QUEUE_DEPTH - 1);
  }
  base_frame frames[2];
  init_frame(&frames[0], make_consumer_frame(&test_queue, ref), CONSUMER);
  init_frame(&frames[1], make_producer_frame(&test_queue, ref), PRODUCER);
  execute_schedule(schedule, len, frames, NUM_THREADS, CONTEXT_SWITCHES_BOUND);
  return 0;
}
