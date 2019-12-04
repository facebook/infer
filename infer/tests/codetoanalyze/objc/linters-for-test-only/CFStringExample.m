/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

static CFStringRef CreateFoo() {
  return CFStringCreateWithCString(NULL, "Hello", kCFStringEncodingUTF8);
}
static _Nullable CFStringRef CreateNullableFoo() {
  return CFStringCreateWithCString(NULL, "Hello", kCFStringEncodingUTF8);
}

static void doSomething(CFStringRef s) { CFShow(s); }

static void doSomethingElse(int x, CFStringRef s) { CFShow(s); }

static void Foo() {}

int main() {
  doSomething(CreateFoo());
  doSomethingElse(0, CreateNullableFoo());
  Foo();
  return 0;
}
