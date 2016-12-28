char *safealloc(int n);

void while_loop(){
  int i = 0;
  char *a = safealloc(10);
  while(*(a + i) && i < 10)	/* BUG */
    a[i++] = 1;			/* SAFE */
}
