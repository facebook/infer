/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#include "MethodCall.h"


int main() {
    MethodCall *call = [MethodCall alloc];
    int n = [call plusX : 1 andY: 3];
    int *x = malloc(sizeof(int));
    return n;
}

int call_nslog() {
  MethodCall *call = [MethodCall alloc];
  NSLog(@"%s", "printing");
  int *x = malloc(sizeof(int));
  return 0;
}
