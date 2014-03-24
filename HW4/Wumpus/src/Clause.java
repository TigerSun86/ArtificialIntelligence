import java.util.ArrayList;

public class Clause {
    public final ArrayList<String> posLiteralSet;
    public final ArrayList<String> negLiteralSet;

    public Clause(final String sentence) {
        posLiteralSet = getPosLiterals(sentence);
        negLiteralSet = getNegLiterals(sentence);
    }

    public Clause(final ArrayList<String> posSet,
            final ArrayList<String> negSet) {
        this.posLiteralSet = posSet;
        this.negLiteralSet = negSet;
    }

    public static Clause resolve (final Clause clause1, final Clause clause2) {
        // Get all positive and negative literals separately first
        final ArrayList<String> newPosLiteralSet = new ArrayList<String>();
        final ArrayList<String> newNegLiteralSet = new ArrayList<String>();
        // Do not add redundant literals like a|a.
        for (String literal : clause1.posLiteralSet) {
            if (!newPosLiteralSet.contains(literal)) {
                newPosLiteralSet.add(literal);
            }
        }
        for (String literal : clause2.posLiteralSet) {
            if (!newPosLiteralSet.contains(literal)) {
                newPosLiteralSet.add(literal);
            }
        }
        for (String literal : clause1.negLiteralSet) {
            if (!newNegLiteralSet.contains(literal)) {
                newNegLiteralSet.add(literal);
            }
        }
        for (String literal : clause2.negLiteralSet) {
            if (!newNegLiteralSet.contains(literal)) {
                newNegLiteralSet.add(literal);
            }
        }
        // Find one pair of complementary literals
        String foundLiteral = null;
        boolean foundOnePair = false;
        boolean moreThanOnePair = false;
        for (String posLiteral : newPosLiteralSet) {
            if (newNegLiteralSet.contains(posLiteral)) {
                // Found one pair of complementary literals
                if (!foundOnePair) {
                    // Found the first pair of complementary literals
                    foundOnePair = true;
                    /* The positive and negative literals are identical, so we
                     * just store one of them. */
                    foundLiteral = posLiteral;
                } else {
                    // Found more than one pair of complementary literals
                    moreThanOnePair = true;
                }
            }
        }

        final Clause resolvedClause;
        if (foundOnePair && !moreThanOnePair) {
            /* There's just one pair of complementary literals between two
             * clauses. So now we will generate the valid resolved clause. */
            newPosLiteralSet.remove(foundLiteral);
            newNegLiteralSet.remove(foundLiteral);
            resolvedClause = new Clause(newPosLiteralSet, newNegLiteralSet);
        } else {
            /* There are more than one pair of complementary literals, then the
             * resolved clause will be tautology and useless, so get rid of it.
             * Or there is no pair, so we cannot get resolved clause neither */
            resolvedClause = null;
        }
        return resolvedClause;
    }

    public final boolean isEmpty () {
        return posLiteralSet.isEmpty() && negLiteralSet.isEmpty();
    }

    public static boolean isSubSet (final ArrayList<Clause> subSet,
            final ArrayList<Clause> uniSet) {
        for (Clause clauseInSubSet : subSet) {
            if (!uniSet.contains(clauseInSubSet)) {
                // There's one clause not in uniSet
                // System.out.println(clauseInSubSet.sentence);
                return false;
            }
        }
        return true;
    }

    public static void add (final ArrayList<Clause> desSet,
            final ArrayList<Clause> srcSet) {
        // Add clauses from srcSet to desSet. Do not add redundant clauses.
        final ArrayList<Clause> newSet = new ArrayList<Clause>();
        for (Clause clauseInSrcSet : srcSet) {
            if (!desSet.contains(clauseInSrcSet)) {
                // It's a new clause
                newSet.add(clauseInSrcSet);
            }
        }
        desSet.addAll(newSet);
    }

    @Override
    public final boolean equals (final Object object) {
        if (!(object instanceof Clause)) {
            return false;
        }

        final Clause anotherClause = (Clause) object;
        for (String posLiteral : posLiteralSet) {
            if (!anotherClause.posLiteralSet.contains(posLiteral)) {
                return false;
            }
        }
        for (String negLiteral : negLiteralSet) {
            if (!anotherClause.negLiteralSet.contains(negLiteral)) {
                return false;
            }
        }
        for (String posLiteral : anotherClause.posLiteralSet) {
            if (!posLiteralSet.contains(posLiteral)) {
                return false;
            }
        }
        for (String negLiteral : anotherClause.negLiteralSet) {
            if (!negLiteralSet.contains(negLiteral)) {
                return false;
            }
        }

        return true;
    }

    @Override
    public final String toString () {
        final StringBuffer sBuffer = new StringBuffer();

        for (String posLiteral : posLiteralSet) {
            sBuffer.append(posLiteral);
            // Add one empty space between literals
            sBuffer.append(" ");
        }
        // Add negative literals
        for (String negLiteral : negLiteralSet) {
            sBuffer.append("!");
            sBuffer.append(negLiteral);
            // Add one empty space between literals
            sBuffer.append(" ");
        }
        if (sBuffer.length() > 0) {
            // Delete the redundant space at the end
            sBuffer.deleteCharAt(sBuffer.length() - 1);
        }
        return sBuffer.toString();
    }

    private static ArrayList<String> getPosLiterals (final String sentence) {
        final String[] literalSet = sentence.split(" ");
        final ArrayList<String> posLiterals = new ArrayList<String>();

        for (String literal : literalSet) {
            if (!literal.isEmpty() && literal.charAt(0) != '!') {
                // It's a positive atom
                posLiterals.add(literal);
            }
        }
        return posLiterals;
    }

    private static ArrayList<String> getNegLiterals (final String sentence) {
        final String[] literalSet = sentence.split(" ");
        final ArrayList<String> negLiterals = new ArrayList<String>();
        for (String literal : literalSet) {
            if (!literal.isEmpty() && literal.charAt(0) == '!') {
                // It's a negative atom
                // Get rid of the negative sign, just store literal
                negLiterals.add(literal.replace("!", ""));
            }
        }
        return negLiterals;
    }
}
