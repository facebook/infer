/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

#include <assert.h>

struct MyPoint {
    int x;
};

int test (struct MyPoint* activeq) {
    assert(activeq != 0);
    return activeq->x;
}
