/**
 * To easily get operators and operands from a expression sentence one by one.
 * 
 * Before use, you should set up operators and their priority and type, and then
 * set the expression sentence.
 * No custom operator could have priority equal or less than MIN_PRI!
 * By type it means how many operands can this operator has.
 * 
 * Operand string is compatible with any character as long as it's not an
 * operator or white space.
 */

import java.util.HashMap;

public class ExpressionAnalyzer {
    public static final int MAX_PRI = Integer.MAX_VALUE;
    // No custom operator could have priority equal or less than MIN_PRI!
    public static final int MIN_PRI = 0;
    // Store custom operators and their priorities
    private final HashMap<String, Integer> operators;
    // Store how many operands can custom operator have
    private final HashMap<String, Integer> operatorType;
    // The expression to analyze
    private String expression;
    // The current position of analyzing expression
    private int position;

    public ExpressionAnalyzer() {
        operators = new HashMap<String, Integer>();
        operators.put("(", MIN_PRI); // Lowest priority
        operators.put(")", MAX_PRI); // Highest priority

        operatorType = new HashMap<String, Integer>();
    }

    public final void addOperator (final String operator, final int priority,
            final int oprType) {
        operators.put(operator, priority);
        operatorType.put(operator, oprType);
    }

    public final boolean isOperator (final String operator) {
        return operators.containsKey(operator);
    }

    public final int getPriority (final String operator) {
        return operators.get(operator);
    }

    public final int getOperatorType (final String operator) {
        return operatorType.get(operator);
    }

    public final void setExpression (final String exp) {
        expression = exp;
        resetPosition();
    }

    public final void resetPosition () {
        position = 0;
    }

    public final boolean hasNext () {
        skipSpace();
        return position < expression.length();
    }

    // Return the end index + 1 of the next element.
    public final String getNext () {
        /* If there's no next element, return null. hasNext method will skip all
         * white space first */
        if (!hasNext()) {
            return null;
        }

        // Check whether the element in current position is an operator
        String element = getOperator(position);
        if (element == null) {
            // It is operand
            int offset = 0;
            while (isInTheBound(position + offset)
                    && isOperand(position + offset)) {
                // Find the end index + 1 of this operand
                offset++;
            }
            element = expression.substring(position, position + offset);

        }
        position += element.length();
        return element;
    }

    @Override
    public final String toString () {
        return expression;
    }

    private String getOperator (final int pos) {
        String foundOpr = null;
        // Check whether the element in this position is an operator.
        for (String opr : operators.keySet()) {
            final int endPos = pos + opr.length();
            assert endPos != pos;
            if (isInTheBound(endPos - 1)
                    && expression.substring(pos, endPos).equals(opr)) {
                // The next element is an operator, so return that.
                foundOpr = opr;
                break;
            }
        }
        return foundOpr;
    }

    private boolean isOperand (final int pos) {
        /* If that position is neither operator nor white space, it's a
         * operand. */
        return ((getOperator(pos) == null) && !Character
                .isWhitespace(expression.charAt(pos)));
    }

    private void skipSpace () {
        int offset = 0;
        while (isInTheBound(position + offset)
                && Character.isWhitespace(expression.charAt(position + offset))) {
            offset++;
        }
        position += offset;
    }

    private boolean isInTheBound (final int pos) {
        return (pos < expression.length());
    }
}
