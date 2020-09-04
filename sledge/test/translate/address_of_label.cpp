/* from http://blog.llvm.org/2010/01/address-of-label-and-indirect-branches.html
 */

static int fn(const char* opcodes) {
  static const void* codetable[] = {&&RETURN, &&INCREMENT, &&DECREMENT,
                                    &&DOUBLE, &&SWAPWORD};
  int result = 0;

  goto* codetable[*(opcodes++)];
RETURN:
  return result;
INCREMENT:
  result++;
  goto* codetable[*(opcodes++)];
DECREMENT:
  result--;
  goto* codetable[*(opcodes++)];
DOUBLE:
  result <<= 1;
  goto* codetable[*(opcodes++)];
SWAPWORD:
  result = (result << 16) | (result >> 16);
  goto* codetable[*(opcodes++)];
}

int main() {
  char opcodes[10];

  return fn(opcodes);
}
