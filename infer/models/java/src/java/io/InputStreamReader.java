package java.io;

import com.facebook.infer.models.InferUndefined;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;

public class InputStreamReader extends Reader {

    private InputStream in;
    private boolean endOfInput;
    private CharsetDecoder decoder;
    private ByteBuffer bytes;

    public InputStreamReader(InputStream in) {
        this.in = in;
    }

    public InputStreamReader(InputStream in, String charsetName)
            throws UnsupportedEncodingException {
        if (charsetName == null)
            throw new NullPointerException("charsetName");
        else if (charsetName == "UTF8" || charsetName == "UTF-8"
                || charsetName == "US-ASCII" || charsetName == "ISO-8859-1"
                || charsetName == "UTF-16BE" || charsetName == "UTF-16LE"
                || charsetName == "UTF-16") {
            this.in = in;
        } else
            throw new UnsupportedEncodingException();
    }

    public InputStreamReader(InputStream in, Charset cs) {
        this.in = in;
    }

    public InputStreamReader(InputStream in, CharsetDecoder dec) {
        this.in = in;
    }

    public void close() throws IOException {
        in.close();
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char cbuf[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char[] cbuf, int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public boolean ready() throws IOException {
        return InferUndefined.can_throw_ioexception_boolean();
    }


}
