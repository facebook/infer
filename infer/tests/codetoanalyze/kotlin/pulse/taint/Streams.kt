/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink
import java.io.IOException
import java.io.InputStream

class Streams {

  var bufferSize = 1024

  @Throws(IOException::class)
  fun copyBad() {
    val tainted = inferSecretSource() as InputStream
    inferSensitiveSink(read(tainted.toString().toByteArray()))
  }

  @Throws(IOException::class)
  fun copyBadFN() {
    val tainted = inferSecretSource() as InputStream
    inferSensitiveSink(read(tainted))
  }

  @Throws(IOException::class)
  fun copyBad1FN() {
    val tainted = inferSecretSource() as InputStream
    val data = ByteArray(24)
    tainted.read(data)
    inferSensitiveSink(data)
  }

  @Throws(IOException::class)
  fun systemArrayCopyBadFN() {
    val tainted = inferSecretSource() as InputStream
    val data = read(tainted.toString().toByteArray())
    val buffer = ByteArray(bufferSize)
    System.arraycopy(data, 0, buffer, 0, data.size)
    inferSensitiveSink(buffer)
  }

  @Throws(IOException::class)
  fun read(`is`: InputStream): ByteArray {
    return read(`is`.toString().toByteArray())
  }

  fun read(data: ByteArray): ByteArray {
    val buffer = ByteArray(bufferSize)
    for (i in data.indices) {
      buffer[i] = data[i]
    }
    return buffer
  }
}
