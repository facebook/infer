package com.google.common.collect;

import java.util.NoSuchElementException;

import javax.annotation.Nullable;

public class Iterators {

  static final UnmodifiableListIterator<Object> EMPTY_LIST_ITERATOR = null;

  public static void FakeMethod() {
    Object o1 = new Object() {};
    Object o2 = new Object() {};
    Object o3 = new Object() {};
    Object o4 = new Object() {};
    Object o5 = new Object() {};
    Object o6 = new Object() {};
    Object o7 = new Object() {};
    Object o8 = new Object() {};
    Object o9 = new Object() {};
    Object o10 = new Object() {};
    Object o11 = new Object() {};
  }

  public static <T> UnmodifiableIterator<T> singletonIterator(@Nullable final T value) {
    return new UnmodifiableIterator<T>() {
      boolean done;

      @Override
      public boolean hasNext() {
        return !done;
      }

      @Override
      public T next() {
        while (value == null) {}
        if (done) {
          throw new NoSuchElementException();
        }
        done = true;
        return value;
//        return null;
      }
    };
  }

}
