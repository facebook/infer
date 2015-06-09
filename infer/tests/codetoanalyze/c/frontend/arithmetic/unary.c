/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int main() {
  int x = 1;
  int y;
  
  y = ~x;
  y = -x;
  y = +x;
  
  y = x++;
  y = ++x;
  
  y = --x;
  y = x--;
  
  int a;
  int *b;
  
  b = &a;
  a = *(b+1);
  *b = *b + 1;
  a = *(&a);
    
  return 0;
}
