/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class AnonymousBox {
  public Object f;
}

@ScopeType(value = Outer.class)
public class OuterHoldsInner<T> {
  // Simple error, requires basic analysis of types, fields, and annotations.
  public final InnerScopedClass u_bad = OuterHoldsInner.getMethod();

  // An error that requires recognizing scope generating methods.
  public final Box<InnerScopedClass> l_bad = InnerScope.getBox(InnerScopedClass.class);

  // An error that requires analyzing the code in the constructor.
  public final Object o_bad = OuterHoldsInner.getMethod();

  public Object f_good = new Object();

  public Object indirect_bad = new Object();

  public Object[] arr_bad = new Object[1];

  // An error that requires taking annotations on interfaces into account.
  public final ExtendsInnerClassViaInterface f_interface = new ExtendsInnerClassViaInterface();

  // An error that requires taking super types into account.
  public final ExtendsInnerScopedClass ex_bad = new ExtendsInnerScopedClass();

  // An error that requires analyzing arrays.
  public final InnerScopedClass[] InnerArr_bad = new InnerScopedClass[5];

  // An error that requires interprocedural analysis to consider chains of fields.
  public final MiddleInnerScopedClass middle_bad = new MiddleInnerScopedClass();

  // An error that requires modeling reflection-based factory methods.
  public final Object FN_inner_via_factory = Factory.make(InnerScopedClass.class);

  public Object anon_box_holder_bad;

  public OuterHoldsInner() {
    AnonymousBox b = new AnonymousBox();
    b.f = new InnerScopedClass();
    // An error that requires tracking field assignments to variables other
    // than 'this' ('b' in this case).
    anon_box_holder_bad = b;
  }

  public static InnerScopedClass getMethod() {
    return new InnerScopedClass();
  }
}

class IndirectAssign {
  public IndirectAssign() {
    OuterHoldsInner a = new OuterHoldsInner();
    // An erroneous assignment to an object of a different class than this one.
    a.indirect_bad = new InnerScopedClass();

    // An erroneous assignment via an array.
    OuterHoldsInner[] arr = new OuterHoldsInner[1];
    arr[0].indirect_bad = new InnerScopedClass();

    // An erroneous assignment via an array. Currently, a false negative.
    a.arr_bad[0] = new InnerScopedClass();
  }
}

class OuterHoldsInnerEx<T> extends OuterHoldsInner<T> {
  // An error that requires analysis this type's super types.
  public final InnerScopedClass xu_bad = OuterHoldsInner.getMethod();

  // An error that requires analysis of the field's super types.
  public final ExtendsInnerScopedClass xus_bad = new ExtendsInnerScopedClass();

  // An error that requires analyzing the annotations of interfaces.
  public final ExtendsInnerClassViaInterface f_interface_bad = new ExtendsInnerClassViaInterface();
}

@ScopeType(value = Inner.class)
class InnerScopedClass {
  public InnerScopedClass() {}
}

class ExtendsInnerScopedClass extends InnerScopedClass {
  public ExtendsInnerScopedClass() {}
}

@ScopeType(value = Inner.class)
interface InnerInterface {}

class ExtendsInnerClassViaInterface implements InnerInterface {
  public ExtendsInnerClassViaInterface() {}
}

class MiddleInnerScopedClass {
  public final InnerScopedClass f = new InnerScopedClass();

  public MiddleInnerScopedClass() {}
}
