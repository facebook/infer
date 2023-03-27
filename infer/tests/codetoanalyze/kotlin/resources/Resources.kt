/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

fun readFileLineByLineUsingForEachLineOk(fileName: String) =
    File(fileName).forEachLine { println(it) }

fun FP_InputOutputStreamOk(fileName: String) {
  val fos = FileOutputStream(fileName)
  val fis = FileInputStream(fileName)
  fis.use { input -> fos.use { output -> output.write(input.read()) } }
}

fun inputOutputStreamBad(fileName: String) {
  val fos = FileOutputStream(fileName)
  val fis = FileInputStream(fileName)
  fis.use { input -> fos.write(input.read()) }
}
