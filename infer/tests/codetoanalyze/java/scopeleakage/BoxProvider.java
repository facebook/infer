/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

public class BoxProvider {

  // Should generate scope
  public static Box get(String name, ScopedClass scoped) {
    return new Box(name);
  }

  // Should not generate scope since its parameters aren't scoped
  public static Box get(String name) {
    return new Box(name);
  }

  // Should not generate scope since its name is not in generator config
  public static Box getBox(String name) {
    return new Box(name);
  }
}
