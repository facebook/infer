/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int main() {
  int k = 0;
  for (int i=0; i<10; i++) {
    for (int j=0; j<10; j++) {
      k = k + i;
    }
  }
  return k;
}
