/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern double sqrt(double);
extern double pow(double, double);

class Form {
 private:
  double area;

 public:
  int color;
  virtual void vmethod() {}

  double getArea() { return this->area; }

  void setArea(double area) { this->area = area; }
};

class Circle : public Form {
 public:
  void vmethod() override {}
  double getRatio() {
    double a;
    a = getArea();
    return sqrt(a / 2 * 3.14);
  }

  void setRatio(double diameter) { setArea(pow(diameter * 0.5, 2) * 3.14); }

  bool isDark() { return (color > 10); }
};
