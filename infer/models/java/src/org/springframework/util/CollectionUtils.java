package org.springframework.util;

import java.util.Collection;
import java.util.Map;

public final class CollectionUtils {

  public static boolean isEmpty(final Collection c) {
    return c == null || c.size() == 0;
  }

  public static boolean isEmpty(final Map m) {
    return m == null || m.size() == 0;
  }
}
