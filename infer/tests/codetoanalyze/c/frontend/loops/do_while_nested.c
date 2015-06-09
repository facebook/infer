/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int main () {
  int a = 10;
  int b = 0;
  do {
    a = 1;
    do {
      a = 2;
    }while( b < 30 );
  }while( b < 20 );
  
  return 0;
}
