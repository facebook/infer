/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import codetoanalyze.java.pulse.InferTaint;

public class Lambdas {

    Function<Integer, String> createFunctionWithTaintedParam() {
        Object object = InferTaint.inferSecretSource();
        return n -> {
            InferTaint.inferSensitiveSink(object);
            return String.valueOf(n);
        };
    }

    String invokeFunction(Function<Integer, String> function) {
        return function.apply(42);
    }

    String FN_createAndInvokeFunctionBad() {
        Function<Integer, String> function = createFunctionWithTaintedParam();
        return invokeFunction(function);
    }
}
