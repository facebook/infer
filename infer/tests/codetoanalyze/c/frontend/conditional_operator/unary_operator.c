/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

void dereference_ifthenelse(int *p) {
  int x;
  x = * (1 ? p : p);

  int y = * (1 ? p : p);

  * (1 ? p : p);
}
