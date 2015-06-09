package java.util.zip;

import com.facebook.infer.models.InferUndefined;

import java.io.IOException;
import java.io.InputStream;

public class GZIPInputStream extends InflaterInputStream {

    private static int FCOMMENT;
    private static int FEXTRA;
    private static int FHCRC;
    private static int FNAME;

    public static int GZIP_MAGIC;
    protected CRC32 crc;
    protected boolean eos;

    public GZIPInputStream(InputStream in, int size) throws IOException {
        super(in);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public GZIPInputStream(InputStream in) throws IOException {
        super(in);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public void close() throws IOException {
        super.close();
    }
}
