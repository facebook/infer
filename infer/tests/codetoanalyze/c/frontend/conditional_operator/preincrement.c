/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

struct s {
  int x;
};

void preincrement(struct s *p) {
  p->x += 1;
  (1 ? p : p)->x += 1;
  p->x += 1 ? 3 : 7;
  (1 ? p : p)->x += 1 ? 3 : 7;
}
