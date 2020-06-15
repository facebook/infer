Reported when an address pointing into the stack of the current
function will escape to its calling context. Such addresses will
become invalid by the time the function actually returns so are
potentially dangerous.

For example, directly returning a pointer to a local variable:

```C
int* foo() {
   int x = 42;
   return &x; // <-- warn here that "&x" will escape
}
```
