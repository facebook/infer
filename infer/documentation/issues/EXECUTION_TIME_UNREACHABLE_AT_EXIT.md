This issue type indicates that the program's execution doesn't reach
the exit node (where our analysis computes the final cost of the
procedure). Hence, we cannot compute a static bound for the procedure.


Examples:
```java
void exit_unreachable() {
  exit(0); // modeled as unreachable
}

void infeasible_path_unreachable() {
    Preconditions.checkState(false); // like assert false, state pruned to bottom
}
```
