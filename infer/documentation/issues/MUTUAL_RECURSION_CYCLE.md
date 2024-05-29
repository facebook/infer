A recursive call or mutually recursive call has been detected. This does *not* mean that the program won't terminate, just that the code is recursive. You should double-check if the recursion is intended and if it can lead to non-termination or a stack overflow.

Example of recursive function:


```C
int factorial(int x) {
  if (x > 0) {
    return x * factorial(x-1);
  } else {
    return 1;
  }
}
```
