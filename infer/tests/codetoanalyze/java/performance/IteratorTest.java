/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.lang.reflect.Field;
import java.util.Iterator;

class MyClassWithFields {
  int x;
  int y;
}

public class IteratorTest {

  public void appendTo(Iterator<?> parts) {
    while (parts.hasNext()) {
      System.out.println(parts.next());
    }
  }

  public void linearIterable(Iterable<?> elements) {
    appendTo(elements.iterator());
  }

  enum Color {
    RED,
    GREEN,
    BLUE;

    // The cost of class initializer should be constant.
    static {
      for (Color c : Color.values()) {}
    }
  }

  public void enum_iter() {
    for (Color c : Color.values()) {}
  }

  public void fields_iter() {
    for (Field f : MyClassWithFields.class.getFields()) {}
  }
}
