struct X_struct {
int a;
int b;
};

class X_class {
public:
int a;
int b;
};

void test() {
  // use pointers until c++ constructors are translated
  X_struct *xs;
  xs->a = 10;
  xs->b = 20;

  X_class *xc;
  xc->a = 10;
  xc->b = 20;
}
