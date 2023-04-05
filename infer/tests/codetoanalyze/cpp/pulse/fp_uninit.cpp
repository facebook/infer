#include <cassert>

void check_range(bool range_var_valid, int& range_var) {
  if (range_var_valid) {
    range_var = range_var + 1;
  }

  assert(range_var_valid >= 0 && range_var_valid <= 1);
}

void do_stuff(bool range_var_valid, int& range_var) {
  check_range(range_var_valid, range_var);
}

int main() {
  bool range_var_valid = false;
  int range_var;
  do_stuff(range_var_valid, range_var);
  return 0;
}
