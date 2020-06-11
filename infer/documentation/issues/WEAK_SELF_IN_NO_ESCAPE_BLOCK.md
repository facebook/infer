In many methods that take a block as an argument, the block position is
annotated with NS_NOESCAPE to mark that the block passed to this method won't be
leaving the current scope. In those cases, there is no need to use `weakSelf` to
avoid the block to capture `self`. This issue type flags this case.
