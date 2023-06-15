This is reported when Infer detects an unnecessary temporary copy of an intermediate object where copy is created to be passed down to a function unnecessarily. Instead, the intermediate object should either be moved into the callee or the type of the callee's parameter should be made `const &`.

A prime example of this occurs when we call a function with a call-by-value parameter as follows:

```cpp
void callee(ExpensiveObject obj) {
  // ....
}

void caller() {
  callee(myExpensiveObj); // a copy of myExpensiveObj is created
  // the copy is destroyed right after the call  
}
```

In this case, when we call `callee`, under the hood, a copy of the argument `myExpensiveObj` is created to be passed to the function call. However, the copy might be unnecessary if

 -   `callee` doesn’t modify its parameter → then we can change its type to `const ExpensiveObject&`, getting rid of the copy at caller
 -   even if `callee` might modify the object, if the argument `myExpensiveObj` is never used later on, we can get rid of the copy by moving it instead: `callee(std::move(myExpensiveObj))`.


The analysis is careful about suggesting moves blindly though: if the argument `myExpensiveObj` is of type `const & ExpensiveObject` then we also recommend that for move to work, const-reference needs to be removed.


PS: We check for other conditions on the argument here: e.g. it should be local to the procedure, as moving a non-local member might cause other memory correctness issues like use-after-move later on.