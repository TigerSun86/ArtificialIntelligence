/**
 * To extract CNF clauses from a CNF sentence.
 * For example, sentence "a (b P[1,2]) !d" could be extracted out 3 clauses:
 * "a", "b P[1,2]", "!d".
 * 
 * Usage example:
 * 
 * CNFExtractor cnfClauses = new CNFExtractor(cnfSentence);
 * while (cnfClauses.hasNext()) {
 * // Extract the clause
 * String clause = cnfClauses.getNext();
 * // do something
 * }
 * 
 */
public class CNFExtractor {
    // The expression to analyze
    private String expression;
    // The current position of analyzing expression
    private int position;

    public CNFExtractor(final String exp) {
        expression = exp;
        resetPosition();
    }

    public final void resetPosition () {
        position = 0;
    }

    // Return the end index + 1 of the next element.
    public final boolean hasNext () {
        skipSpace();
        return position < expression.length();
    }

    public final String getNext () {
        /* If there's no next element, return null. hasNext method will skip all
         * white space first */
        if (!hasNext()) {
            return null;
        }
        final String element;
        int offset = 0;
        if (expression.charAt(position) == '(') {
            // It's a list
            while (isInTheBound(position + offset)
                    && expression.charAt(position + offset) != ')') {
                // Find the end index + 1 of this list
                offset++;
            }
            // Just need the content inside the '(' and )'
            element = expression.substring(position + 1, position + offset);
            // Make the pointer move over the ')'
            offset++;
        } else {
            // It's a atom
            while (isInTheBound(position + offset) && isAtom(position + offset)) {
                // Find the end index + 1 of this atom
                offset++;
            }
            element = expression.substring(position, position + offset);
        }
        /* Here we need update position by offset but not element length,
         * because offset include the length of ')' */
        position += offset;
        return element;
    }

    private boolean isAtom (final int pos) {
        /* If that position is neither '(' nor '!' nor white space, it's a
         * atom. */
        final char c = expression.charAt(pos);
        return (c != '(' && c != '!' && !Character.isWhitespace(c));
    }

    // Skip all blank space
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
