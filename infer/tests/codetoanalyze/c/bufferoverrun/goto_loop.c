void goto_loop() {
  int i = 0;
  char a[10];

loop_start:
  if (i >= 10)
    goto loop_end;
  a[i] = 'a'; /* SAFE */
  i++;
  goto loop_start;
loop_end:
  a[i] = 'a'; /* BUG */
}
