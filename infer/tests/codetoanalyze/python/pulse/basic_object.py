# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import level1

class A:
    _f1 : int

    def __init__(self, f1: int) -> None:
        self._f1 = f1

    def getField1(self) -> int:
        return self._f1

    def setField1(self, f1: int) -> None:
        self._f1 = f1

class B(A):
    pass

class C(B):
    _f2 : int


    def __init__(self, f1: int, f2: int) -> None:
        super().__init__(f1)
        self._f2 = f2

    def getField2(self) -> int:
        return self._f2

    def setField2(self, f2: int) -> None:
        self._f2 = f2

### TODO: The Python Front-End does not yet support @classmethod
###
### class Base:
###     # no __ConsistentConstruct in Python so all subclasses of Base must NOT
###     # declare any constructor, so they all will bubble back up to
###     # Base.__init__ and share the same signature
###     def __init__(self, f: int) -> None:
###         self.f = f
###
###     # (vsiles)
###     # `@classmethod` enables us to encode Hack's `new static` construct,
###     # but I'm not yet aware of an equivalent of the `this` type so we'll keep
###     # these untyped
###     @classmethod
###     def make(cls, f: int):
###         return cls(f)
###
###     def copy(self):
###         return self.__class__(self.f)
###
### class Derived(Base):
###     # NO __init__ so Derived has the same constructor signature as Base
###
###     def copy(self):
###         return super().copy()
###


def set_and_get_A_bad(u: int, v: int) -> None:
    tainted = level1.taintSource()
    a = A(u)
    i0 = a.getField1()
    a.setField1(v)
    i = a.getField1()
    if (i0 == u and i == v):
        level1.taintSink(tainted)

def set_and_get_A_good(u: int, v: int) -> None:
    tainted = level1.taintSource()
    a = A(u)
    i0 = a.getField1()
    a.setField1(v)
    i = a.getField1()
    if (i0 != u or i != v):
        level1.taintSink(tainted)

# testing constuctor inheritance
def set_and_get_B_bad(u: int, v: int) -> None:
    tainted = level1.taintSource()
    a = B(u)
    i0 = a.getField1()
    a.setField1(v)
    i = a.getField1()
    if (i0 == u and i == v):
        level1.taintSink(tainted)

def set_and_get_B_good(u: int, v: int) -> None:
    tainted = level1.taintSource()
    a = B(u)
    i0 = a.getField1()
    a.setField1(v)
    i = a.getField1()
    if (i0 != u or i != v):
        level1.taintSink(tainted)

# testing super().__init__() call
def set_and_get_C_bad(u: int, v: int, x: int, y: int) -> None:
    tainted = level1.taintSource()
    a = C(u,v)
    i0 = a.getField1()
    j0 = a.getField2()
    a.setField1(x)
    i = a.getField1()
    a.setField2(v)
    j = a.getField2()
    if (i0 == u and j0 == v and i == x and j == v):
        level1.taintSink(tainted)

def set_and_get_C_good(u: int, v: int, x: int, y: int) -> None:
    tainted = level1.taintSource()
    a = C(u,v)
    i0 = a.getField1()
    j0 = a.getField2()
    a.setField1(x)
    i = a.getField1()
    a.setField2(v)
    j = a.getField2()
    if (i0 != u or j0 != v or i != x or j != v):
        level1.taintSink(tainted)

#   // testing static() constructor
#   // see https://docs.hhvm.com/hack/attributes/predefined-attributes#__consistentconstruct
#   public function set_and_get_Derived_bad(int $u, int $v): void {
#     $tainted = \Level1\taintSource();
#     $o = Derived::make($u);
#     $i0 = $o->f;
#     $o->f = $v;
#     $i = $o->f;
#     if ($i0 == $u && $i == $v) {
#       \Level1\taintSink($tainted);
#     }
#   }

#   public function set_and_get_Derived_good(int $u, int $v): void {
#     $tainted = \Level1\taintSource();
#     $o = Derived::make($u);
#     $i0 = $o->f;
#     $o->f = $v;
#     $i = $o->f;
#     if ($i0 != $u || $i != $v) {
#       \Level1\taintSink($tainted);
#     }
#   }

#   public function set_and_get_Derived_copy_bad(int $u, int $v, int $w): void {
#     $tainted = \Level1\taintSource();
#     $o1 = Derived::make($u);
#     $o2 = $o1->copy();
#     $i0 = $o1->f;
#     $o1->f = $v;
#     $i = $o2->f;
#     $o2->f = $w;
#     if ($i0 == $u && $o1->f == $v && $i == $u && $o2->f == $w) {
#       \Level1\taintSink($tainted);
#     }
#   }

#   public function set_and_get_Derived_copy_good(int $u, int $v, int $w): void {
#     $tainted = \Level1\taintSource();
#     $o1 = Derived::make($u);
#     $o2 = $o1->copy();
#     $i0 = $o1->f;
#     $o1->f = $v;
#     $i = $o2->f;
#     $o2->f = $w;
#     if ($i0 != $u || $o1->f != $v || $i != $u || $o2->f != $w) {
#       \Level1\taintSink($tainted);
#     }
#   }
# }
