package lib;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class Framework {

    public static Object returnNull() {
        return null;
    }

    private static String source() {
        return "AttackerControlled";
    }

    public static String getString() {
        return source();
    }

    public static String readFile(String s) throws IOException {
        try (FileInputStream inputStream = new FileInputStream(s)) {
            String content = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
            return content;
        }
    }

    public static String propagates(String s) {
        return s;
    }

    enum Value {
        Left, Right
    }

    public static String doesNotPropagate(String s) {
        Value v = Value.valueOf(s);
        switch (Value.valueOf(s)) {
            case Left:
                return "Left";
            case Right:
                return "Right";
            default:
                return "Unknown";
        }
    }


}
