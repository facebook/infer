package java.io;

import com.facebook.infer.models.InferUndefined;

public class FilterOutputStream extends OutputStream {

    protected OutputStream out;

    public FilterOutputStream() {
    }

    public FilterOutputStream(OutputStream out) {
        this.out = out;
    }

    public void close() throws IOException {
        if (out != null) {
            if (out instanceof FileOutputStream) {
                ((FileOutputStream) out).close();
            }
        }
    }

    public void flush() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(int b) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }


}
