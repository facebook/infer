/* pulse-inf: works -- empty loop conditions */
void simple_loop_break() {
  int y = 0;
  while (y < 100)
    {
      y++;
      if (y == 50)
	break;
      y--;
    }
}
