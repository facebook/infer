/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

public class Factory {
  public static <T> T make(Class<T> c) {
    T result = null;
    try {
      result = c.getDeclaredConstructor().newInstance();
    } catch (NoSuchMethodException e) {
    } catch (java.lang.reflect.InvocationTargetException e) {
    } catch (InstantiationException e) {
    } catch (IllegalAccessException e) {
    }
    return result;
  }
}
