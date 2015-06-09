package java.lang;

import com.facebook.infer.models.InferBuiltins;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Properties;

public final class System {

    private System() {
    }

    public final static InputStream in;

    static {
        byte[] arr = {0};
        in = new ByteArrayInputStream(arr);
    }

    public final static PrintStream out = new PrintStream(
            new ByteArrayOutputStream());

    public final static PrintStream err = new PrintStream(
            new ByteArrayOutputStream());

    private static Properties systemProperties;

    private static String lineSeparator;

    public static void exit(int status) {
        InferBuiltins._exit();
    }

}
