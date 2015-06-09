

package java.net;

import com.facebook.infer.models.InferBuiltins;

public class HttpURLConnection extends URLConnection {
    private static int DEFAULT_CHUNK_LENGTH;

    private static String[] PERMITTED_USER_METHODS;

    protected String method;

    protected int responseCode;

    protected String responseMessage;

    protected boolean instanceFollowRedirects;

    private static boolean followRedirects;

    protected int chunkLength;

    protected int fixedContentLength;

    protected long fixedContentLengthLong;

    public static int HTTP_ACCEPTED;

    public static int HTTP_BAD_GATEWAY;

    public static int HTTP_BAD_METHOD;

    public static int HTTP_BAD_REQUEST;

    public static int HTTP_CLIENT_TIMEOUT;

    public static int HTTP_CONFLICT;

    public static int HTTP_CREATED;

    public static int HTTP_ENTITY_TOO_LARGE;

    public static int HTTP_FORBIDDEN;

    public static int HTTP_GATEWAY_TIMEOUT;

    public static int HTTP_GONE;

    public static int HTTP_INTERNAL_ERROR;

    public static int HTTP_LENGTH_REQUIRED;

    public static int HTTP_MOVED_PERM;

    public static int HTTP_MOVED_TEMP;

    public static int HTTP_MULT_CHOICE;

    public static int HTTP_NO_CONTENT;

    public static int HTTP_NOT_ACCEPTABLE;

    public static int HTTP_NOT_AUTHORITATIVE;

    public static int HTTP_NOT_FOUND;

    public static int HTTP_NOT_IMPLEMENTED;

    public static int HTTP_NOT_MODIFIED;

    public static int HTTP_OK;

    public static int HTTP_PARTIAL;

    public static int HTTP_PAYMENT_REQUIRED;

    public static int HTTP_PRECON_FAILED;

    public static int HTTP_PROXY_AUTH;

    public static int HTTP_REQ_TOO_LONG;

    public static int HTTP_RESET;

    public static int HTTP_SEE_OTHER;

    public static int HTTP_SERVER_ERROR;

    public static int HTTP_USE_PROXY;

    public static int HTTP_UNAUTHORIZED;

    public static int HTTP_UNSUPPORTED_TYPE;

    public static int HTTP_UNAVAILABLE;

    public static int HTTP_VERSION;


    public HttpURLConnection(URL url) {
        super(url);
        method = new String();
        InferBuiltins.__set_file_attribute(method);
    }


    public void disconnect() {
        InferBuiltins.__set_mem_attribute(method);
    }
}
