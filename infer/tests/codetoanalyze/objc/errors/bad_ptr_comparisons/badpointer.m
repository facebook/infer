/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSArray.h>

void bad1() {

    NSNumber *n2;
    int i;

    if (!n2) {
        i = 0;
    } else {
        i = 1;
    }
}


void bad2(NSArray *a, NSNumber *n) {
    int i = 0;
    for (; !n; ) {
        i = n.intValue;
    }
}

void bad3(NSArray *a, NSNumber *n) {
    int i = 0;
    while (!n) {
        i = n.intValue;
    }
}


void good1(NSArray *a) {
    int i = 0;
    for (NSNumber *n in a) {
        i = n.intValue;
    }
}

void good2 () {

    NSNumber *n2;
    int i;

    if (n2 != nil) {
        i = 1;
    } else {
        i = 0;
    }
}
