/**
 * A Generator of postfix
 * */
import java.util.ArrayDeque;

public class Postfix {
    public static final String getPostfixbyInfix (final ExpressionAnalyzer expression) {
        final ArrayDeque<String> oprStack = new ArrayDeque<String>();
        final ArrayDeque<String> ansStack = new ArrayDeque<String>();

        // Loop until the end of infix
        while (expression.hasNext()) {
            // Extract next element from infix
            final String newElement = expression.getNext();
            if (expression.isOperator(newElement)) {
                // It's an operator
                dealWithOperator(oprStack, ansStack, newElement, expression);
            } else {
                // It's an operand, just push it into ansStack
                ansStack.push(newElement);
            }
        }
        // Push remain operators
        while (!oprStack.isEmpty()) {
            ansStack.push(oprStack.pop());
        }

        // Generate postfix string for output
        final StringBuffer sBuffer = new StringBuffer();
        while (!ansStack.isEmpty()) {
            // Remove in inverse order
            sBuffer.append(ansStack.removeLast());
            sBuffer.append(" ");
        }
        if (sBuffer.length() > 0) {
            sBuffer.deleteCharAt(sBuffer.length() - 1);
        }
        return sBuffer.toString();
    }

    private static void dealWithOperator (final ArrayDeque<String> oprStack,
            final ArrayDeque<String> ansStack, final String newOpr,
            final ExpressionAnalyzer expression) {
        if (newOpr.equals("(")) {
            oprStack.push(newOpr);
        } else if (newOpr.equals(")")) {
            /* Pop all operators above '(', and push them into posStack and
             * get rid of ( and ) */
            while (true) {
                final String opr = oprStack.pop();
                if (opr.equals("(")) {
                    break;
                }
                ansStack.push(opr);
            }
        } else {
            /* Other operators.
             * There should never be an operator with MIN_PRI dealt in this
             * part, it will cause pop from empty stack. */
            final int newPriority = expression.getPriority(newOpr);
            while (true) {
                final int peekPriority;
                if (oprStack.isEmpty()) {
                    peekPriority = ExpressionAnalyzer.MIN_PRI;
                } else {
                    peekPriority = expression.getPriority(oprStack.peek());
                }
                if (newPriority > peekPriority) {
                    // New operator has bigger priority
                    oprStack.push(newOpr);
                    break;
                } else {
                    /* Move the low and equal priority operators to the
                     * posStack */
                    ansStack.push(oprStack.pop());
                }
            }
        } // if (newOpr.equals("(")) {
    }
}
