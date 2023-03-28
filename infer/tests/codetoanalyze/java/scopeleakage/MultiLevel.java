/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@ScopeType(value = Outer.class)
public class MultiLevel<T> {

  // An error that requires interprocedural analysis to consider chains of fields.
  public final Level3 f3_bad = new Level3();
  // public final Level2 f2_bad = new Level2();
  // public final Level1 f1_bad = new Level1();
  // public final Leaf f0_leaf_bad = new Leaf();
}

@ScopeType(value = Inner.class)
class Leaf {
  public Leaf() {}
}

class Level1 {
  public final Leaf f1 = new Leaf();

  public Level1() {}
}

class Level2 {
  public final Level1 f2 = new Level1();
}

class Level3 {
  public final Level2 f3 = new Level2();
}
