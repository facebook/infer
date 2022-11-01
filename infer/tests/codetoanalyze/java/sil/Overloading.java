//
// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
//

class Overloading{
  int i;
  double f;

  public Overloading(int i, double f) {
    this.i = i;
    this.f = f;
  }

  public Overloading(int i) {
    this(i, 0.0);
  }

  public Overloading(double f) {
    this(0, f);
  }

  public Overloading() {
    this(0, 0);
  }

  public static Overloading make(int i) {
    return new Overloading(i);
  }

  public static Overloading make(double f) {
    return new Overloading(f);
  }

  public static Overloading make() {
    return new Overloading();
  }

}
