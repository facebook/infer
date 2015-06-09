package java.io;

import com.facebook.infer.models.InferUndefined;

public class BufferedOutputStream extends FilterOutputStream {

    public BufferedOutputStream(OutputStream out) {
        super(out);
    }

    public BufferedOutputStream(OutputStream out, int size) {
        super(out);
    }

    public void close() throws IOException {
        if (out != null) {
            out.close();
        }
    }

    public void flush() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(int b) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

}
