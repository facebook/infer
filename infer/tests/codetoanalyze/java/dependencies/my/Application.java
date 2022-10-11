/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package my;

import lib.Framework;

public class Application {

    String indirectNPE() {
        return Framework.returnNull().toString();
    }

    void indirectTainFlow() {
        String s = Framework.getString();
        Framework.readFile(s);
    }

    String source() {
        return "AttackerControlled";
    }

    String propagate(String s) {
        return s;
    }

    void sink(String s) {
        // do critical thing with s
    }

    void simpleTaintExampe() {
        sink(propagate(source()));
    }

    void shouldReportTaintPropagation() {
        sink(Framework.shouldPropagateTaint(source()));
    }

    void shouldNotReportTaintPropagation() {
        sink(Framework.doesNotPropagateTaint(source()));
    }

}
