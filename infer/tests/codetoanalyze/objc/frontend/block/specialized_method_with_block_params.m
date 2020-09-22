/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
@interface A

typedef void (^MyBlock)(int x);

@end

int p() { return 0; }

foo(_Nullable MyBlock my_block1, _Nullable MyBlock my_block2, _Nonnull A* a) {
  my_block1(22);
}

@implementation A {

  int x;
}

- (int)bar:(A*)a {
  int x = 0;
  foo(
      ^(int i) {
        self->x = x;
      },
      ^(int i) {
        self->x = i;
      },
      a);
  int y = p();
  return self->x;
}

- (int)bar2 {
  foo(
      ^(int i) {
        self->x = 5;
      },
      ^(int i) {
        self->x = 5;
      },
      self);
  return self->x;
}

- (int)call_foo_with_same_param {
  void (^b)(int) = ^(int i) {
    self->x = 5;
  };
  foo(b, b, self);
  foo(b, b, self);
  return self->x;
}

@end
