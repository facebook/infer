/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

enum week{ sunday, monday, tuesday, wednesday=0, thursday, friday, saturday};

int main(){
  enum week today;
  today=wednesday;
  today=monday;
  today=today+4;
  today=(enum week) tuesday+1;
  int i = tuesday+(friday-sunday);
  return 0;
}
