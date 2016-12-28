int break_continue_return(){
  int i = 0;
  char a[10];

  while(1){
    i++;
    if(i >= 10) break;
    if(i < 2) continue;
    a[i] = 'a';			/* SAFE */
  }
  i = 0;
  while(1){
    if(i > 10) return 0;
    a[i] = 'a';			/* BUG */
    i++;
  }
  return 0;
}
