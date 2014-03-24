import java.util.ArrayList;

public class Attribute {
    private final Object name;
    private final ArrayList<Object> valueList;

    public Attribute(final Object nameIn) {
        this.name = nameIn;
        this.valueList = new ArrayList<Object>();
    }

    public final void add (final Object value) {
        this.valueList.add(value);
    }

    public final void addAll (final ArrayList<Object> valueListIn) {
        this.valueList.addAll(valueListIn);
    }

    public final Object getName () {
        return name;
    }

    public final ArrayList<Object> valueList () {
        return valueList;
    }

    @Override
    public final boolean equals (final Object attr) {
        if (!(attr instanceof Attribute)) {
            return false;
        }

        return name.equals(((Attribute) attr).getName());
    }

    @Override
    public final int hashCode () {
        return name.hashCode();
    }

    @Override
    public final String toString () {
        return name.toString() + valueList;
    }
}
