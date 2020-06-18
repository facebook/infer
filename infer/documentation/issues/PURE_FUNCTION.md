This issue type indicates pure functions. For instance, below functions would be marked as pure:

```java
int local_write_pure(int x, int y) {
  int k = x + y;
  k++;
  return k;
}

// no change to outside state, the local allocation is ok.
int local_alloc_pure(ArrayList<Integer> list) {
  ArrayList<Integer> list_new = new ArrayList<Integer>();
  for (Integer el : list) {
    list_new.add(el);
  }
  return list_new.size();
}
```

However, the following ones would not be pure:

```java
void swap_impure(int[] array, int i, int j) {
  int tmp = array[i];
  array[i] = array[j]; // modifying the input array
  array[j] = tmp;
}

int a = 0;
void set_impure(int x, int y) {
  a = x + y; //modifying a global variable
}
```
