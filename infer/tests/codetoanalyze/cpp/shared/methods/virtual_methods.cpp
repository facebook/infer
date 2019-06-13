/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Polygon {
 protected:
  int width, height;

 public:
  virtual ~Polygon();
  void set_values(int a, int b) {
    width = a;
    height = b;
  }
  virtual int area() { return 0; }
};

class Rectangle : public Polygon {
 public:
  ~Rectangle();
  int area() { return width * height; }
};

class Triangle : public Polygon {
 public:
  ~Triangle(){};
  int area() {
    int x = width * height;
    return x - 10;
  }
};

int rect_area() {
  Rectangle rect;
  Polygon* ppoly1 = &rect;
  ppoly1->set_values(4, 5);
  return 1 / (ppoly1->area() - 20);
}

int tri_area() {
  Triangle trgl;
  Polygon poly;
  Polygon* ppoly2 = &trgl;
  ppoly2->set_values(4, 5);
  return 1 / (ppoly2->area() - 10);
}

int poly_area() {
  Polygon poly;
  Polygon* ppoly3 = &poly;
  return 1 / ppoly3->area();
}

int tri_not_virtual_area() {
  Triangle trgl;
  Polygon poly;
  Polygon* ppoly2 = &trgl;
  ppoly2->set_values(4, 5);
  return 1 / ppoly2->Polygon::area();
}

// dynamic dispatch in this case still doesn't work,
// one need special dealing with names for destructors.
// But the call gets the virtual flag.
void call_virtual_destructor() {
  Polygon* trgl = new Triangle;
  delete trgl;
}
