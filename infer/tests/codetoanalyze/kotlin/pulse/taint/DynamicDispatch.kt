/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink

class DynamicDispatch {

  interface Interface {
    fun returnSource(): Any?

    fun callSink(o: Any?)

    fun propagate(o: Any?): Any?
  }

  class BadInterfaceImpl1 : Interface {
    override fun returnSource(): Any? {
      return inferSecretSource()
    }

    override fun callSink(o: Any?) {
      inferSensitiveSink(o)
    }

    override fun propagate(o: Any?): Any? {
      return o
    }
  }

  class BadInterfaceImpl2 : Interface {
    override fun returnSource(): Any? {
      return inferSecretSource()
    }

    override fun callSink(o: Any?) {
      inferSensitiveSink(o)
    }

    override fun propagate(o: Any?): Any? {
      return o
    }
  }

  class OkInterfaceImpl : Interface {
    override fun returnSource(): Any? {
      return null
    }

    override fun callSink(o: Any?) {}

    override fun propagate(o: Any?): Any? {
      return null
    }
  }

  open class Supertype {
    open fun returnSource(): Any? {
      return null
    }

    open fun callSink(o: Any?) {}

    open fun propagate(o: Any?): Any? {
      return null
    }
  }

  class BadSubtype : Supertype() {
    override fun returnSource(): Any? {
      return inferSecretSource()
    }

    override fun callSink(o: Any?) {
      inferSensitiveSink(o)
    }

    override fun propagate(o: Any?): Any? {
      return o
    }
  }

  /**
   * interface tests. for all of these, we should see a warning for both BadInterfaceImpl1 and
   * BadInterfaceImpl2, but not OkInterfaceImpl
   */
  fun FN_returnSourceViaInterfaceBad(i: Interface) {
    val source = i.returnSource()
    inferSensitiveSink(source)
  }

  fun FN_callSinkViaInterfaceBad(i: Interface) {
    val source = inferSecretSource()
    i.callSink(source)
  }

  fun propagateViaInterfaceBad(i: Interface) {
    val source = inferSecretSource()
    val launderedSource = i.propagate(source)
    inferSensitiveSink(launderedSource)
  }

  fun interfaceOk() {
    val i: Interface = OkInterfaceImpl()
    val source1 = i.returnSource()
    inferSensitiveSink(source1)
    val source2 = inferSecretSource()
    i.callSink(source2)
    val launderedSource = i.propagate(source2)
    inferSensitiveSink(launderedSource)
  }

  fun FN_returnSourceViaSubtypeBad(s: Supertype) {
    val source = s.returnSource()
    inferSensitiveSink(source)
  }

  fun FN_callSinkViaSubtypeBad(s: Supertype) {
    val source = inferSecretSource()
    s.callSink(source)
  }

  fun FN_propagateViaSubtypeBad(s: Supertype) {
    val source = inferSecretSource()
    val launderedSource = s.propagate(source)
    inferSensitiveSink(launderedSource)
  }

  // need to look and see if we know the concrete type of the receiver to get this one
  fun propagateViaConcreteTypeOk() {
    val s = Supertype()
    val source1 = s.returnSource()
    inferSensitiveSink(source1)
    val source2 = inferSecretSource()
    s.callSink(source2)
    val launderedSource = s.propagate(source2)
    inferSensitiveSink(launderedSource)
  }
}
