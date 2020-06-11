This will happen in one of two cases generally:

1. One uses `weakSelf` but forgot to declare it weak first.
2. One is using `strongSelf`, declared in a block, in another (inside) block.
   This changes the delicate balance of the `weakSelf`/`strongSelf` use in the
   first block. The retain cycle is avoided there because `strongSelf` is a
   local variable to the block. If `strongSelf` is used in the inside block,
   then it's not a local variable anymore, but a captured variable.
