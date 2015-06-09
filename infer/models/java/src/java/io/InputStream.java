package java.io;

import com.facebook.infer.models.InferUndefined;

public class InputStream implements Closeable {

    private int MAX_SKIP_BUFFER_SIZE;

    public void close() throws IOException {
        if (this instanceof FileInputStream) {
            ((FileInputStream) this).close();
        } else if (this instanceof FilterInputStream) {
            ((FilterInputStream) this).close();
        }
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[], int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

}
