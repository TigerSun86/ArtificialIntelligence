import java.util.ArrayList;
import java.util.Collection;

public class Example {
    private final ArrayList<Object> example;

    public Example() {
        this.example = new ArrayList<Object>();
    }

    public final ArrayList<Object> getExample () {
        return example;
    }

    public final Object get (final int index) {
        return example.get(index);
    }

    public final void add (final Object value) {
        example.add(value);
    }

    public final void addAll (final Collection<Object> values) {
        example.addAll(values);
    }

    public final int size () {
        return example.size();
    }

    public final boolean isEmpty () {
        return example.isEmpty();
    }

    public final boolean checkValue (final int index, final Object value) {
        return value.equals(example.get(index));
    }

    @Override
    public final boolean equals (final Object otherExample) {
        if (!(otherExample instanceof Example)) {
            return false;
        }
        final Example ex2 = (Example) otherExample;
        if (this.size() != ex2.size()) {
            return false;
        }
        for (int i = 0; i < this.size(); i++) {
            final Object valueInEx2 = ex2.get(i);
            if (!this.get(i).equals(valueInEx2)) {
                return false;
            }
        }
        return true;
    }

    private static final int PRIME = 7;
    private static final int INIT = 3;

    @Override
    public final int hashCode () {
        int hash = INIT;
        for (Object value : example) {
            hash = PRIME * hash + value.hashCode();
        }
        return hash;
    }

    @Override
    public final String toString () {
        final StringBuffer sBuffer = new StringBuffer();

        for (Object value : example) {
            sBuffer.append(value.toString());
            sBuffer.append(" ");
        }
        if (sBuffer.length() != 0) {
            // Delete the redundant " ".
            sBuffer.deleteCharAt(sBuffer.length() - 1);
        }
        return sBuffer.toString();
    }
}
