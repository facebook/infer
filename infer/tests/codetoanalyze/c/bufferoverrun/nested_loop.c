void nested_loop() {
  int i, j;
  char a[10];

  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* SAFE */
    for (j = 0; j <= 10; j++) {
      a[j] = 'a'; /* BUG */
    }
  }
}
