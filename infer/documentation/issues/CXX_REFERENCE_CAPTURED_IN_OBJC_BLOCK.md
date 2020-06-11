With this check, Infer detects C++ references captured in a block. Doing this is
almost always wrong. The reason is that C++ references are not managed pointers
(like ARC pointers) and so the referent is likely to be gone by the time the
block gets executed. One solution is to do a local copy of the reference and
pass that to the block. Example:

```c
(int &) v;
...
const int copied_v = v;
^{
// use copied_v not v
};
```
