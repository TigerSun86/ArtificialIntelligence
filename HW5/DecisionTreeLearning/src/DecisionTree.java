import java.util.Collection;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

public class DecisionTree {
    // Root is an attribute or a class.
    private Object root;
    // The first is a key, "a value of one attribute".
    // The second is a subtree.
    private HashMap<Object, DecisionTree> branches;

    public DecisionTree(final Object root2) {
        this.root = root2;
        this.branches = new HashMap<Object, DecisionTree>();
    }

    public final Object getRoot () {
        return root;
    }

    public final void setRoot (final Object root2) {
        this.root = root2;
    }

    public final boolean isLeaf () {
        return branches.isEmpty();
    }

    public final Set<Entry<Object, DecisionTree>> branchSet () {
        return branches.entrySet();
    }

    /**
     * Here "value" means "the value of the attribute",
     * but not the HashMap value."
     */
    public final Set<Object> valueSet () {
        return branches.keySet();
    }

    public final Collection<DecisionTree> subTreeSet () {
        return branches.values();
    }

    /**
     * Here "value" means "the value of the attribute",
     * but not the HashMap value."
     */
    public final DecisionTree getSubTree (final Object value) {
        return branches.get(value);
    }

    /**
     * Here "value" means "the value of the attribute",
     * but not the HashMap value."
     */
    public final void
            addBranch (final Object value, final DecisionTree subTree) {
        branches.put(value, subTree);
    }

    @Override
    public final String toString () {
        // Print using pre-order traversal.
        final StringBuffer sBuffer = new StringBuffer();
        sBuffer.append("[");
        sBuffer.append(root.toString());

        for (Entry<Object, DecisionTree> branch : branches.entrySet()) {
            final Object value = branch.getKey();
            final DecisionTree subTree = branch.getValue();
            sBuffer.append(" ");
            sBuffer.append(value.toString());
            sBuffer.append(subTree.toString()); // Recursively.
        }
        sBuffer.append("]");
        return sBuffer.toString();
    }
}
