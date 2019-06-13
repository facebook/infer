/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface A : NSObject

- (int)foo:(int)foo_par;

@end

@implementation A

- (int)foo:(int)foo_par {

  return foo_par;
}

@end

@interface B : A // Error: A subclass

- (int)bar;

@end

@implementation B

- (int)bar {
  A* a = [[A alloc] init];
  return [a foo:5]; // Error: report MACRO_TEST1, MACRO_TEST2, MACRO_TEST3
}

@end

@interface C : B // Error: C subclass of B subclass of A
@end

@implementation C
@end

@interface D : C // Error: D subclass of C ... subclass of A
@end

@implementation D
@end

@interface E : D // Error: E subclass of D ... subclass of A
@end

@implementation E
@end

@interface F : NSObject
@end

@implementation F
@end

@interface ThisIsAVeryLongName : NSObject
@end

@implementation ThisIsAVeryLongName
@end

@interface TestType : NSObject

- (void)m1;
- (bool)m2;
- (char)m3;
- (unsigned char)m4;
//- (wchar_t) m5;
//- (char16_t) m6;
- (unsigned short)m7;
- (unsigned int)m8;
- (unsigned long)m9;
- (unsigned long long)m10;
- (__int128)m11;
- (unsigned __int128)m12;
- (signed char)m13;
- (short)m14;
- (int)m15;
- (long)m16;
- (long long)m17;
//- (half) m18;
- (float)m20;
- (double)m21;
- (long double)m22;
//- (__float128) m23;
//- (nullptr_t) m24;
//- (id) m25;
//- (Class) m26;
//- (SEL) m27;
- (float*)m23;
- (unsigned int**)m24;
- (A*)m25;
- (ThisIsAVeryLongName*)m26;
- (void)m26:(int)p1
     pname2:(float)p2
     pname3:(ThisIsAVeryLongName*)p3
     pname4:(A*)p4;
@end

typedef unsigned long my_ulong;

my_ulong l;

typedef struct {
  int a;
  int b;
  int c;
} S, *my_pS;

my_pS p;

typedef struct Node {
  struct my_listNode* next;
} my_listNode;

my_listNode ln;

void foo(int n, int m) {}

void bar() { foo(0, 2); }
