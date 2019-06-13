/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
bool star();

class T {
 public:
  int x;
  T* field;

 public:
  int* _Nullable mayReturnNullPointer() {
    if (star()) {
      return nullptr;
    } else {
      return new int;
    }
  }

 public:
  T* _Nullable mayReturnNullObject() {
    if (star()) {
      return nullptr;
    } else {
      return this;
    }
  }

 public:
  T* doesNotReturnNullObject() { return new T(); }

 public:
  void doSomething() {}
};

void assignNullableValueBad(T* t) {
  int* p = t->mayReturnNullPointer();
  *p = 42;
}

void reassigningNullablePointerOkay(T* t) {
  int* p = t->mayReturnNullPointer();
  p = new int; // does not report here
  *p = 42; // does not report here
}

void reassigningNullablePointerToNullOkay(T* t) {
  int* p = t->mayReturnNullPointer();
  p = nullptr; // does not report here
}

void callMethodOnNullableObjectBad(T* t) {
  t->mayReturnNullObject()->doSomething();
}

void callMethodOnNullableObjectOkay(T* t) {
  T* p = t->mayReturnNullObject();
  if (p != nullptr) {
    p->doSomething();
  }
}

void dereferenceFieldOfNullableObjectBad(T* t) {
  T* p = t->mayReturnNullObject();
  p->x = 42;
}

void methodCallOnFieldOfNullableObjectBad(T* t) {
  T* p = t->mayReturnNullObject();
  p->field->doSomething();
}

void avoidDoubleReportingBad(T* t) {
  T* p = t->mayReturnNullObject();
  p->doSomething(); // reports here
  p->doSomething(); // does not report here
}

void nullableAssignmentInOneBranchBad(T* t) {
  T* p;
  if (star()) {
    p = t->mayReturnNullObject();
  } else {
    p = t->doesNotReturnNullObject();
  }
  p->doSomething(); // reports here
}

void methodCheckedForNullOkay(T* t) {
  if (t->mayReturnNullObject() != nullptr) {
    t->mayReturnNullObject()->doSomething(); // does not report here
  }
}

void methodCheckedForNullAndReturnOkay(T* t) {
  if (t->mayReturnNullObject() == nullptr) {
    return;
  }
  t->mayReturnNullObject()->doSomething(); // does not report here
}

void reportsViolationOutsideOfNullCheckBad(T* t) {
  if (t->mayReturnNullObject() != nullptr) {
    t->mayReturnNullObject()->doSomething(); // does not report here
  }
  t->mayReturnNullObject()->doSomething(); // reports here
}

void reportsViolationInNullBranchBad(T* t) {
  if (t->mayReturnNullObject() == nullptr) {
    t->mayReturnNullObject()->doSomething(); // reports here
  }
}

void reportsViolationInNotNullElseBranchBad(T* t) {
  if (t->mayReturnNullObject() != nullptr) {
  } else {
    t->mayReturnNullObject()->doSomething(); // reports here
  }
}

void methodAlwaysCheckedForNullOkay(T* t) {
  if (star() && t->mayReturnNullObject() != nullptr) {
    t->mayReturnNullObject()->doSomething(); // does not report here
  }
}

void methodNotAlwaysCheckedForNullBad(T* t) {
  if (star() || t->mayReturnNullObject() != nullptr) {
    t->mayReturnNullObject()->doSomething(); // reports here
  }
}

void onlyReportOnceBad(T* t) {
  t->mayReturnNullObject()->doSomething(); // reports here
  // ...
  t->mayReturnNullObject()->doSomething(); // does not report here
}

void dereferenceOfAliasesCheckedForNullOkay(T* t) {
  T* s = t->mayReturnNullObject();
  T* r = s;
  if (r != nullptr) {
    s->doSomething();
  }
}

void pointerTestOkay(T* t) {
  T* p = t->mayReturnNullObject();
  if (p) {
    p->doSomething();
  }
}

void pointerTestBad(T* t) {
  T* p = t->mayReturnNullObject();
  if (p) {
    // ...
  }
  p->doSomething();
}

void methodTestedForNullOkay(T* t) {
  if (t->mayReturnNullObject()) {
    t->mayReturnNullObject()->doSomething();
  }
}
