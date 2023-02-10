/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import kotlin.reflect.KClass
import kotlin.reflect.KProperty

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.RUNTIME)
annotation class ScopeType(val value: KClass<*>)

class Inner {}

class Outer {}

@ScopeType(value = Inner::class) class InnerScopedClass {}

class InnerScope {}

@ScopeType(value = Outer::class)
public class OuterHoldsInner {
  private val f_bad: InnerScopedClass
  private val g_bad: InnerScopedClass = InnerScopedClass()
  private val delegate_FN: InnerScopedClass by InnerScopedClassDelegate()

  init {
    this.f_bad = InnerScopedClass()
  }
}

class InnerScopedClassDelegate {
  operator fun getValue(thisRef: Any?, property: KProperty<*>): InnerScopedClass {
    return InnerScopedClass()
  }
}

@ScopeType(value = Outer::class)
class AnotherOuterHoldsInner {
  private val f_bad: InnerScopedClass = InnerScopedClass()
}
