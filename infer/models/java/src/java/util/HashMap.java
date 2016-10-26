/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util;
import java.io.*;

import com.facebook.infer.builtins.InferUndefined;

/**
 * A recency abstraction for hashmaps that remembers only the last two
 * keys known to exist in the map.
 *
 * get(key) can return null when key is not in the hashmap, and
 * containsKey(key) and put(key, value) are used to protect against
 * such nulls. Slightly unsound for the other reason get() can return
 * null, when a pair (key,null) is in the map.  Then when
 * containsKey(key) is true, we will not report an NPE on a subsequent
 * get(key).
*/

public abstract class HashMap<K,V> extends AbstractMap<K,V>
  implements Map<K,V>, Cloneable, Serializable {

  private Object lastKey1 = null;
  private Object lastKey2 = null;

  public boolean containsKey(Object key) {
    // doesn't actually check if _containsKey(key). If you just put a
    // key in the map, why would you check if it's still there?

    if (InferUndefined.boolean_undefined()) {
      pushKey(key);
      return true;
    } else {
      return false;
    }
  }

  public V get(Object key) {
    if (_containsKey(key)) {
      return (V)InferUndefined.object_undefined();
    } else if (InferUndefined.boolean_undefined()) {
      pushKey(key);
      return (V)InferUndefined.object_undefined();
    }

    return null;
  }

  public V put(K key, V value) {
    pushKey(key);

    if (InferUndefined.boolean_undefined()) {
      return (V)InferUndefined.object_undefined();
    }
    return null;
  }


  /** some sort of circular buffer simulator */
  private void pushKey(Object key) {
    lastKey2 = lastKey1;
    lastKey1 = key;
  }

  private boolean _containsKey(Object key) {
      return areEqual(key, lastKey1) || areEqual(key, lastKey2);
  }

  /* does explicit dynamic dispatch to help Infer out */
  private static boolean areEqual(Object x, Object y) {
      return x == y
          || (x instanceof Integer && ((Integer) x).equals(y));
  }

}
