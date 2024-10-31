/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/Foundation.h>

@class SelfInBlockTest;

@interface SelfInBlockTestUser

- (void)use_self_in_block_test:(SelfInBlockTest*)test;

- (void)use_self_in_block_test_nullable:(int)x
                                    and:(_Nullable SelfInBlockTest*)test;
@end

@interface A

@property(nonatomic, strong) id image;

@end

@interface SelfInBlockTestSuper : NSObject

@property(nonatomic, weak) NSString* userSession;

@end

@interface SelfInBlockTest : SelfInBlockTestSuper

@property(nonatomic, weak) SelfInBlockTestUser* user;

- (void)foo;

- (void)bar;

- (A*)process:(A*)obj;

@end

void m(SelfInBlockTest* obj) {}

void m2(_Nullable SelfInBlockTest* obj) {}

@implementation SelfInBlockTest {
  int x;
  NSString* _name;
}

- (void)foo {
}

- (void)bar {
}

- (A*)process:(A*)obj {
  return obj;
}

- (void)mixSelfWeakSelf_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      int x = self->x; // bug here
      [self foo];
    }
    return 0;
  };
}

- (void)strongSelfNoCheckNotWeakSelf_good {
  __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    return strongSelf->x;
  };
}

- (void)strongSelfNoCheck_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    [strongSelf foo];
    return 0;
  };
}

// very unlikely pattern, but still complies with invariant:
// any use of strongSelf is bad unless checked for null beforehand
- (void)strongSelfNoCheck2_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    __strong __typeof(weakSelf) strongSelf2 = strongSelf;
    return 0;
  };
}

- (void)strongSelfCheckOnce_bad {
  __weak __typeof(self) weakSelf = self;
  void (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      int x = strongSelf->x;
    } else {
      strongSelf->x; // bug here
      [strongSelf foo]; // no bug here
      m(strongSelf); // no bug here because of dedup
    }
  };
}

- (void)strongSelfCheck2_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      int x = strongSelf->x;
    } else {
      m(strongSelf); // bug here
      int x = strongSelf->x;
    }
    return 0;
  };
}

- (void)strongSelfCheck6_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    m2(strongSelf); // bug here
    return 0;
  };
}

- (void)strongSelfCheck3_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (!strongSelf) {
      return 0;
    } else {
      [strongSelf foo]; // no bug here
    }
    return 0;
  };
}

- (void)strongSelfCheck4_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    int y = strongSelf->x;
    if (strongSelf == nil) {
      return 0;
    }
    return strongSelf->x;
  };
}

- (void)strongSelfCheck5_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    [strongSelf.user use_self_in_block_test:strongSelf]; // bug here
    return 0;
  };
}

- (void)strongSelfCheck7_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    [strongSelf.user use_self_in_block_test_nullable:1
                                                 and:strongSelf]; // bug here
    return 0;
  };
}

- (void)strongSelfNotCheck5_good:(A*)a {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf && a.image) {
      int x = strongSelf->x; // no bug here
    }
    return 0;
  };
}

- (void)strongSelfCheck7_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    return strongSelf ? strongSelf->x : 0; // no bug here
  };
}

- (void)weakSelfMultiple_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    [weakSelf foo];
    [weakSelf bar];
    return 0;
  };
}

- (void)weakSelfBranch_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    if (isTapped) {
      [weakSelf foo];
    } else {
      [weakSelf bar];
    }
    return 0;
  };
}

- (void)weakSelfBranch_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    if (isTapped) {
      [weakSelf foo];
    }
    [weakSelf bar];
    return 0;
  };
}

- (void)capturedStrongSelf_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong typeof(self) strongSelf = weakSelf;
    if (strongSelf) {
      int (^my_block)() = ^() {
        int x = strongSelf->x; // bug here
        x = strongSelf->x; // bug here
        return 0;
      };
      int x = strongSelf->x;
    }
    return 0;
  };
}

- (void)capturedStrongSelf_good:(NSArray<A*>*)allResults {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong typeof(self) strongSelf = weakSelf;
    if (strongSelf) {
      [allResults
          enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL* stop) {
            A* result =
                [strongSelf process:obj]; // no bug because of NS_NOESCAPE flag
          }];
    }
    return 0;
  };
}

- (void)mixSelfWeakSelf_good:(NSArray*)resources {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    [self foo]; // no bug here
    int (^my_block)() = ^() {
      [weakSelf foo];
      return 0;
    };
    return 0;
  };
}

// Super is actually self but using it means find the method in the super class.
// So using weakSelf and super is still the same problem, but the fix is less
// clear, so we produce no autofix.
- (void)mixSelfWeakSelf_super_bad_no_autofix {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      id copy = [super copy]; // bug here
      [copy foo];
    }
    return 0;
  };
}

// No autofix because self is used before strongSelf is defined.
// The developer needs to move the use of self to a different line.
- (void)mixSelfWeakSelf_bad_no_autofix {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    [self foo];
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
    }
    return 0;
  };
}

#define _UNWRAP_STRONG_VARIABLE(nullable_ptr)                         \
  ({                                                                  \
    _Pragma("clang diagnostic push");                                 \
    _Pragma("clang diagnostic ignored \"-Wvoid-ptr-dereference\"");   \
    __typeof(*(nullable_ptr))* null_unspecified_ptr = (nullable_ptr); \
    __typeof(null_unspecified_ptr) _Nonnull nonnull_ptr =             \
        null_unspecified_ptr;                                         \
    _Pragma("clang diagnostic pop");                                  \
    nonnull_ptr;                                                      \
  })

#define WEAK_SELF __weak __typeof(*self)* _Nullable weakSelf = self

#define STRONG_VARIABLE_OR_RETURN(weakVariable, strongVariable) \
  __typeof(*weakVariable)* _Nonnull(strongVariable) =           \
      _UNWRAP_STRONG_VARIABLE(weakVariable);                    \
  do {                                                          \
    if (!(strongVariable)) {                                    \
      return;                                                   \
    }                                                           \
  } while (0)

#define STRONG_SELF_OR_RETURN STRONG_VARIABLE_OR_RETURN(weakSelf, strongSelf)

- (void)mixSelfWeakSelf_bad_correct_autofix_macro {
  __weak __typeof(self) weakSelf = self;
  void (^my_block)() = ^() {
    STRONG_SELF_OR_RETURN;
    [strongSelf foo];
    [self foo];
  };
}

#define MY_LOG(format, ...) NSLog(format, ##__VA_ARGS__)

// When the code comes from a macro expansion we can't tell what
// the syntactical line and column is precisely, so we avoid computing autofixes
// in that case.
- (void)mixSelfWeakSelf_bad_no_autofix_macro {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      MY_LOG(@"%@ async load asset completed.", self);
    }
    return 0;
  };
}

- (void)mixSelfWeakSelf_bad_autofix_implicit_self {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      _name = @"Dulma";
      x = 10;
    }
    return 0;
  };
}

// Super is actually self but using it means find the method in the super class.
// So using weakSelf and super is still the same problem, but the fix is less
// clear, so we produce no autofix.
- (void)mixSelfWeakSelf_bad_no_autofix_super_property {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      NSString* s = super.userSession;
    }
    return 0;
  };
}

- (void)strongSelfNoCheck2Instances_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (isTapped) {
      strongSelf->_name = @"Dulma";
    } else {
      strongSelf->_name = @"Alice";
    }
    return 0;
  };
}

- (void)strongSelfNoCheck2InstancesNoIf_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (isTapped) {
      strongSelf->_name = @"Dulma";
      strongSelf->_name = @"Alice";
    }
    return 0;
  };
}

- (void)strongSelfCheck10_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf != nil) {
      [strongSelf foo]; // no bug here
    }
    return 0;
  };
}

- (void)strongSelfCheck11_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf == nil) {
      return 0;
    }
    [strongSelf foo]; // no bug here
    return 0;
  };
}

@end
