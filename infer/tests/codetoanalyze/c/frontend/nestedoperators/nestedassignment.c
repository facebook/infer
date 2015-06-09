/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int main() {
  double x = 1.0;
  double q, r, s, t;
  x = s;
  q = (x = 3);
  x += 7;
  q = (x += 1.0);
  q = (x += (r += (s += t)));
  return 0;
} 
