package java.util.zip;

import com.facebook.infer.models.InferUndefined;

import java.io.*;
import java.util.HashSet;

public class ZipOutputStream extends DeflaterOutputStream {

    public static int DEFLATED;

    public static int STORED;

    private static int ZIP_VERSION_2_0;

    private byte[] commentBytes;

    private HashSet<String> entries;

    private int defaultCompressionMethod;

    private int compressionLevel;

    private ByteArrayOutputStream cDir;

    private ZipEntry currentEntry;

    private CRC32 crc;

    private int offset, curOffset, nameLength;

    private byte[] nameBytes;

    public ZipOutputStream(OutputStream out) {
        super(out);
    }

    public void putNextEntry(ZipEntry e) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void closeEntry() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        if (out != null) {
            if (out instanceof FileOutputStream) {
                ((FileOutputStream) out).close();
            } else if (out instanceof BufferedOutputStream) {
                ((BufferedOutputStream) out).close();
            }
        }
    }
}
