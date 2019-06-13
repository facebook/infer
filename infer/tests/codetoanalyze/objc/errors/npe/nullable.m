/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSString.h>

@interface NullableA : NSObject {
 @public
  int fld;
}

@end

@interface NullableB : NSObject

@property int prop;

- (void)method;

@end

int derefNullableParamDirect(NullableA* __nullable param) { return param->fld; }

int derefNullableParamIndirect(NullableA* __nullable param) {
  NullableA* local = param;
  return local->fld;
}

NullableA* derefNullableParamOk(NullableA* __nullable param) {
  if (!param)
    param = [NullableA new];
  param->fld = 7;
  return param;
}

int parameter_nullable_ok(NSString* body,
                          NSString* __nullable linkTapAction,
                          NullableA* options) {
  return options->fld;
}

int parameter_nullable_bug(NullableA* __nullable options,
                           NSAttributedString* body,
                           NSString* linkTapAction)

{
  return options->fld;
}

int readNullableParamPropertyOk(NullableB* __nullable param) {
  return param.prop;
}

void writeNullableParamPropertyOk(NullableB* __nullable param) {
  param.prop = 7;
}

void methodCallOnNullableParamOk(NullableB* __nullable param) {
  [param method];
}
