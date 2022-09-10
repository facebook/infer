
package my;

import java.io.IOException;
import lib.Framework;

public class Application {

    String indirectNPE() {
        return Framework.returnNull().toString();
    }

    void indirectTainFlow() {
        String s = Framework.getString();
        try {
            String t = Framework.readFile(s);
        } catch (IOException e) {
            // do nothing
        }
    }

    String source() {
        return "AttackerControlled";
    }

    void sink(String s) {
        // do critical thing with s
    }

    String propagates(String s) {
        return s;
    }

    void simpleTaintExampe() {
        sink(propagates(source()));
    }

    void shouldReportTaintPropagation() {
        sink(Framework.propagates(source()));
    }

    void shouldNotReportTaintPropagation() {
        sink(Framework.doesNotPropagate(source()));
    }

}
