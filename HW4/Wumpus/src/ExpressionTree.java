import java.util.ArrayDeque;

class BTree implements Cloneable {
    String data;
    BTree leftChild;
    BTree rightChild;

    public BTree(final String d, final BTree left, final BTree right) {
        data = d;
        leftChild = left;
        rightChild = right;
    }

    @Override
    public final BTree clone () {
        BTree newLChild = null;
        BTree newRChild = null;
        if (this.leftChild != null) {
            newLChild = this.leftChild.clone();
        }
        if (this.rightChild != null) {
            newRChild = this.rightChild.clone();
        }
        return new BTree(this.data, newLChild, newRChild);
    }

    public final String toString () {
        return inorderTraversal(this);
    }

    private static String inorderTraversal (final BTree bTree) {
        if (bTree == null) {
            return "";
        }
        final StringBuffer sBuffer = new StringBuffer();
        if ((bTree.leftChild == null) && (bTree.rightChild == null)) {
            sBuffer.append(bTree.data);
        } else if (bTree.leftChild == null) {
            // Unary operator
            sBuffer.append(bTree.data);
            sBuffer.append(inorderTraversal(bTree.rightChild));
        } else {
            sBuffer.append("(");
            sBuffer.append(inorderTraversal(bTree.leftChild));
            sBuffer.append(" ");
            sBuffer.append(bTree.data);
            sBuffer.append(" ");
            sBuffer.append(inorderTraversal(bTree.rightChild));
            sBuffer.append(")");
        }
        return sBuffer.toString();
    }
}

public class ExpressionTree {
    public static BTree getExpressionTree (final ExpressionAnalyzer expression) {
        final ArrayDeque<BTree> stack = new ArrayDeque<BTree>();
        // Loop until the end of infix
        while (expression.hasNext()) {
            // Extract next element from infix
            final String newElement = expression.getNext();
            // System.out.println(newElement);
            if (expression.isOperator(newElement)) {
                /* It's an operator
                 * Pop two(or one) children for new node. First popped one
                 * should be right child.
                 * Unary operator only has a right child. */
                final BTree rightChild = stack.pop();
                final BTree leftChild;
                final int childrenNum = expression.getOperatorType(newElement);
                if (childrenNum == 2) {
                    leftChild = stack.pop();
                } else {
                    leftChild = null;
                }
                // Generate a node store operator
                final BTree node = new BTree(newElement, leftChild, rightChild);
                // Push new node into stack
                stack.push(node);
            } else {
                // It's an operand
                // Generate a new leaf node
                final BTree leaf = new BTree(newElement, null, null);
                // Push node into stack
                stack.push(leaf);
            }
        }
        return stack.pop();
    }
}
