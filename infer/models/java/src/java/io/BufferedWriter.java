package java.io;

import com.facebook.infer.models.InferUndefined;

public class BufferedWriter extends Writer {

    private Writer out;

    private char[] buf;

    private int pos;

    public BufferedWriter(Writer out) {
        this.out = out;
    }

    public BufferedWriter(Writer out, int sz) {
        this.out = out;
    }

    public void flush() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void newLine() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(char cbuf[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(char cbuf[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(int c) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(String str) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(String str, int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        if (out instanceof OutputStreamWriter) {
            ((OutputStreamWriter) out).close();
        }
    }


}
