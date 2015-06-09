/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

void dereference_in_array_access(int **p) {
  if (p[0]);
  if ((*p)[1]);
  if (p[**p]);
  if ((*p)[**p]);
}
