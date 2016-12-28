void inf_loop(){
  int i = 0;
  char a[10];

  while(1){
    if(i >= 10) i = 0;
    a[i] = 'a';			/* SAFE */
    i++;
  }
}
