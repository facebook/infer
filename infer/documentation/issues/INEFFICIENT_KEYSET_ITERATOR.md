This issue is raised when
- iterating over a HashMap with `ketSet()` iterator
- looking up the key each time

Instead, it is more efficient to iterate over the loop with `entrySet` which returns key-vaue pairs and gets rid of the hashMap lookup.
 For instance, we would raise an issue for the following program:

```java
void inefficient_loop_bad(HashMap<String, Integer> testMap) {
 for (String key : testMap.keySet()) {
   Integer value = testMap.get(key); // extra look-up cost
   foo(key, value);
 }
}
```

Instead, it is more efficient to have:
```java
void efficient_loop_ok(HashMap<String, Integer> testMap) {
  for (Map.Entry<String, Integer> entry : testMap.entrySet()) {
    String key = entry.getKey();
    Integer value = entry.getValue();
    foo(key, value);
  }
}
```
