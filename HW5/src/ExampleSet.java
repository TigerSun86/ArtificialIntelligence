import java.util.HashSet;

public class ExampleSet {
    private final HashSet<Example> eSet;
    private final AttributeList attrList;

    public ExampleSet(final AttributeList attrList2) {
        this.eSet = new HashSet<Example>();
        this.attrList = attrList2;
    }

    public final void add (final Example example) {
        eSet.add(example);
    }

    public final boolean isEmpty () {
        return eSet.isEmpty();
    }

    public final int size () {
        return eSet.size();
    }

    public final HashSet<Example> getExampleSet () {
        return eSet;
    }

    /**
     * public final Object sameClassification ()
     * Returns a certain classification if all examples are the same
     * classification.
     * @return: class, if all examples have the same classification.
     *          null, if examples don't have the same classification or there's
     *          no example.
     */
    public final Object sameClassification () {
        if (eSet.isEmpty()) {
            return null;
        }

        Object sameClass = null;
        for (Object classifi : attrList.classList()) {
            final int num = numberOf(null, null, classifi);
            if (num == eSet.size()) {
                // All examples are this class.
                sameClass = classifi;
                break;
            } else if (num != 0) {
                // There are at least two kinds of classification.
                break;
            }
        }
        return sameClass;
    }

    /**
     * public final Object mode ()
     * Returns the classification occurring the most times in the example set.
     * @return: class occurring the most.
     *          null, if there's no example.
     */
    public final Object mode () {
        int max = 0;
        Object maxClass = null;
        for (Object classifi : attrList.classList()) {
            final int num = numberOf(null, null, classifi);
            if (max < num) {
                max = num;
                maxClass = classifi;
            }
        }
        return maxClass;
    }

    /**
     * public ExampleSet subExampleSet (Attribute attr, Object valueFilter)
     * @return: subExampleList with examples which have specified value of
     *          attribute.
     */
    public final ExampleSet subExampleSet (final Attribute attr,
            final Object valueFilter) {
        assert attr.valueList().contains(valueFilter);
        final int attrIndex = attrList.indexOf(attr);
        assert attrIndex != -1;
        final ExampleSet subExampleList = new ExampleSet(attrList);
        for (Example example : eSet) {
            if (example.checkValue(attrIndex, valueFilter)) {
                subExampleList.add(example);
            }
        }
        return subExampleList;
    }

    /**
     * public int numberOf (Attribute attr, Object valueFilter,
     * Object classFilter)
     * Return number of examples. 3 parameters are filters, can be null.
     * @return: number of examples with specified attribute, value of the
     *          attribute and classification of example.
     */
    public final int numberOf (final Attribute attr, final Object valueFilter,
            final Object classFilter) {
        if (classFilter == null && valueFilter == null) {
            // No filter, return whole size.
            assert attr == null;
            return eSet.size();
        }

        int sum = 0;
        for (Example example : eSet) {
            boolean isClassEqual = true;
            boolean isValueEqual = true;
            if (classFilter != null) {
                isClassEqual =
                        example.checkValue(attrList.indexOfTarget(),
                                classFilter);
            }

            if (isClassEqual && valueFilter != null) {
                final int index = attrList.indexOf(attr);
                isValueEqual = example.checkValue(index, valueFilter);
            }
            if (isClassEqual && isValueEqual) {
                // Pass all the filters, count it.
                sum++;
            }
        }
        return sum;
    }
}
