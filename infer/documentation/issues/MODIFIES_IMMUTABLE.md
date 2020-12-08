This issue type indicates modifications to fields marked as @Immutable. For instance, below function `mutateArray` would be marked as modifying immutable field `testArray`:
```java
  @Immutable int[] testArray = new int[]{0, 1, 2, 4};
  
  int[] getTestArray() {
    return testArray;
  }                
          
  void mutateArray() {
    int[] array = getTestArray();
    array[2] = 7;
  }
```
