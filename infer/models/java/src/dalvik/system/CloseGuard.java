package dalvik.system;

public class CloseGuard {


    private static CloseGuard NOOP;

    private static volatile boolean ENABLED;

    private static volatile Reporter REPORTER;


    public static interface Reporter {
        public void report(String message, Throwable allocationSite);
    }
}
