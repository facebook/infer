package java.lang;

import java.util.List;

public class Thread implements Runnable {

    private static int NANOS_PER_MILLI;

    public static int MAX_PRIORITY;
    public static int MIN_PRIORITY;

    public static int NORM_PRIORITY;

    volatile VMThread vmThread;
    volatile ThreadGroup group;
    volatile boolean daemon;
    volatile String name;
    volatile int priority;
    volatile long stackSize;
    Runnable target;
    private static int count;

    private long id;
    ThreadLocal.Values localValues;
    ThreadLocal.Values inheritableValues;
    private List<Runnable> interruptActions;
    private ClassLoader contextClassLoader;
    private UncaughtExceptionHandler uncaughtHandler;
    private static UncaughtExceptionHandler defaultUncaughtHandler;
    boolean hasBeenStarted;

    private int parkState;
    private Object parkBlocker;

    public static interface UncaughtExceptionHandler {
    }

    public synchronized void start() {
        run();
    }

    public void run() {
        if (target != null) {
            target.run();
        }
    }

}
