/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import codetoanalyze.java.pulse.InferTaint;
import java.util.Map;

public class MapExamples {

    native Map<String, String> split1(Object s);

    void propagateTaintOnMap1Bad_FN() {
        Object object = InferTaint.inferSecretSource();
        Map<String, String> map = split1(object);
        String value = map.get("Whatever");
        InferTaint.inferSensitiveSink(value);  // Taint flow not reported here
    }

    native Map<String, String[]> split2(Object s);

    void propagateTaintOnMap2Bad() {
        Object object = InferTaint.inferSecretSource();
        Map<String, String[]> map = split2(object);
        String value = "";
        if (!map.isEmpty()) {
            String[] whatever = map.get("Whatever");
            if (whatever != null) value = whatever[0];
        }
        InferTaint.inferSensitiveSink(value);
    }

    static void wrapper(String s) {
        InferTaint.inferSensitiveSink(s);
    }

    void propagateTaintOnMap3Bad() {
        Object object = InferTaint.inferSecretSource();
        Map<String, String[]> map = split2(object);
        String value = null;
        if (!map.isEmpty()) {
            String[] whatever = map.get("Whatever");
            if (whatever != null) value = whatever[0];
        }
        wrapper(value);
    }

    void propagateTaintOnMap4Bad_FN() {
        Object object = InferTaint.inferSecretSource();
        Map<String, String[]> map = split2(object);
        String value = "";
        if (!map.isEmpty()) {
            String[] whatever = map.get("Whatever");
            if (whatever != null) value = whatever[0];
        }
        wrapper(value); // Taint flow not reported here
    }
}
