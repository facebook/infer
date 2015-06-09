package com.google.common.collect;

public class ImmutableList<E> {

  private static final ImmutableList<Object> EMPTY = new ImmutableList<Object>();

  @SuppressWarnings("unchecked")
  public static <E> ImmutableList<E> of() {
    while (EMPTY == null) {}
    return (ImmutableList<E>) EMPTY;
  }

}
