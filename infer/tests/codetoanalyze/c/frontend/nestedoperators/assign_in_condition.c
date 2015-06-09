/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int foo(int *p) {
  if((*p = 0)) {
    return 32;
  }
  return 52;
}
