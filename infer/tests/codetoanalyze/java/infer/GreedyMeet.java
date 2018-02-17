package codetoanalyze.java.infer;

import java.util.Random;

class O { int f1, f2, f3, f4; }

public class GreedyMeet {
  static Object a, b, c;

  static void doNothingInAComplicatedWay() {
    if (maybe() && a == b) {
    } else if (maybe() && a == c) {
    }
  }

  static void dereferenceANull(int x) {
    a = b = c = null;
    doNothingInAComplicatedWay();
    a.hashCode(); // should warn (null dereference)
  }

  static int manySpecs(O x) {
    int r = 0;
    if (maybe()) {
      if (x.f1 == 1) r += 10;
      if (x.f1 == 2) r += 20;
      if (x.f1 == 3) r += 30;
    } else if (maybe()) {
      if (x.f2 == 1) r += 100;
      if (x.f2 == 2) r += 200;
      if (x.f2 == 3) r += 300;
    } else if (maybe()) {
      if (x.f3 == 1) r += 1;
      if (x.f3 == 2) r += 2;
      if (x.f3 == 3) r += 3;
    } else if (maybe()) {
      if (x.f4 == 1) r += 1000;
      if (x.f4 == 2) r += 2000;
      if (x.f4 == 3) r += 3000;
    }
    return r;
  }


  static boolean maybe() { return random.nextBoolean(); }
  static Random random = new Random();
}
