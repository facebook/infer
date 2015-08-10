// commented out parts are not done yet
struct A {
  public:
  int member1;
  float member2;
  public:
  int fun(int a, int b);
  // overloading
  int fun(int a, int b, int c);
  int add(const A& other);

  struct AIn {
    int fun1() { return 1;}
    int fun(int a, int b);
  };
  // inline definition
  int def_in() { int c = 10; return c+1;}

  // default parameters
  int fun_default(int a, int b = 10, int c = 20) {return a+b+c;}
  //static function
  //static int get_fun() {return 1;}
};

int A::fun(int a, int b, int c) {
  //using class members
  //fun(a,b);
}

int A::fun(int a, int b) {
  int c  = a + b + 1;
  //using class members
  //member1 = c;
  return c*c;
}

int A::AIn::fun(int a, int b) {
  return a+b;
}

int A::add(const A& other) {
   //member1 += other.member1;
   //return member1;
}

void call_method() {
  // constructing objects
  //A a;
  //a.fun(1,2);

  A *a_ptr;
  // calling methods
  a_ptr->fun(10,20);
  a_ptr->fun_default(1,2,3);
}

void call_method_with_default_parameters() {
  A *a_ptr;
  a_ptr->fun_default(1,2);
  a_ptr->fun_default(1);
}

/*
void call_static_method {
  A *a_ptr;
  A::get_fun();
  a_ptr->get_fun();
}
*/
