/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package my;

import lib.Framework;
import lib.Str;

public class Application {

    int indirectNPE() {
        return Framework.returnNull().hashCode();
    }

    Str source() {
        return Str.ATTACKER_CONTROLLED;
    }

    Str propagate(Str s) {
        return s;
    }

    void sink(Str s) {
        // do critical thing with s
    }

    void indirectSource() {
        sink(propagate(Framework.getStr()));
    }

    void indirectSink() {
        Str s = source();
        Framework.readFile(s);
    }

    void indirectTaintFlow() {
        Str s = Framework.getStr();
        Framework.readFile(s);
    }
}
