/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSString.h>

int* __nullable returnsNull();

@interface T : NSObject
- (void)assignUnnanotatedFieldToNullBad;
@end

@implementation T {
  int* unnanotatedField;
  int* __nullable nullableField;
  int* nonnullField;
}

- (void)assignNullableFieldToNullOkay {
  nullableField = nil;
}

- (void)assignUnnanotatedFieldToNullBad {
  unnanotatedField = nil;
}

- (void)assignNonnullFieldToNullBad {
  nonnullField = nil;
}

- (void)testNullableFieldForNullOkay {
  if (nullableField == nil) {
  }
}

- (void)testUnnanotatedFieldForNullBad {
  if (unnanotatedField == nil) {
  }
}

- (int)DeadStoreFP_testUnnanotatedFieldInClosureBad {
  int (^testField)(int defaultValue);
  testField = ^(int defaultValue) {
    if (unnanotatedField != nil) {
      return *unnanotatedField;
    } else {
      return defaultValue;
    }
  };
  return testField(42);
}

- (void)testNonnullFieldForNullBad {
  if (nonnullField == nil) {
  }
}

- (void)dereferenceUnnanotatedFieldOkay {
  *unnanotatedField = 42;
}

- (void)dereferenceNonnullFieldOkay {
  *nonnullField = 42;
}

- (void)dereferenceNullableFieldBad {
  *nullableField = 42;
}

- (void)dereferenceUnnanotatedFieldAfterTestForNullBad {
  if (unnanotatedField == nil) {
    *unnanotatedField = 42;
  }
}

- (void)FP_dereferenceNonnullFieldAfterTestForNullOkay {
  if (nonnullField == nil) {
    *nonnullField = 42;
  }
}

- (void)dereferenceNullableMethodBad {
  int* p = returnsNull();
  *p = 42;
}

- (void)dereferenceNullableMethod1Ok {
  int* p = returnsNull();
  if (p) {
    *p = 42;
  }
}

- (void)dereferenceNullableMethod2Ok {
  int* p = returnsNull();
  if (p != nil) {
    *p = 42;
  }
}

@end
