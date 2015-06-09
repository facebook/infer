package java.util;

import com.facebook.infer.models.InferUndefined;

public class Vector<E> extends AbstractList<E> {

    private static long serialVersionUID;
    protected int elementCount;
    protected Object[] elementData;
    protected int capacityIncrement;
    private static int DEFAULT_SIZE;

    E elementData(int index) {
        return (E) elementData[index];
    }

    public Enumeration<E> elements() {
        return new Enumeration<E>() {
            int count;

            public boolean hasMoreElements() {
                return count > 0;
            }

            public E nextElement() {
                if (count > 0)
                    return (E) InferUndefined.object_undefined();
                else
                    throw new NoSuchElementException();
            }
        };
    }

    public E get(int index) {
        return elementData(index);
    }

    public int size() {
        return InferUndefined.nonneg_int();
    }

}
