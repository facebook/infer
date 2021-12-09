This checks reports when `nil` is passed to collections in Objective-C such as arrays and dictionaries. This causes a crash.

### Arrays

Adding objects to an array, inserting objects at a given index, or replacing objects at a given index, can all
lead to a crash when the object is `nil`.

```objectivec
  [mArray addObject:nil];  //crash

  [mArray insertObject:nil atIndex:0];   //crash

  [mArray replaceObjectAtIndex:0 withObject:nil]; //crash
```

### Dictionaries

Adding a `nil` value in a dictionary causes a crash. If the concept of `nil` is required, one can add
`[NSNull null]` instead.

```objectivec
  id value = nil;
  [mDict setObject:value forKey:@"somestring"]; //crash

  [mDict setObject:[NSNull null] forKey:@"somestring"]; //ok
```

Retrieving or removing an object from a dictionary with a `nil` key also causes a crash:

```objectivec
    id key = nil;
    mDict[key] = @"somestring"; //crash

   [mDict removeObjectForKey:nil]; //crash
```

**Action**:

In all the cases above, when passing `nil` causes a crash, the solutions are either making sure
that the object passed will never be `nil`, or adding a check for `nil` before calling those methods.
