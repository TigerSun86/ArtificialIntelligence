/**
 * To convert a infix sentence to a CNF sentence.
 * 
 * For example: "B[1,1] <=> (P[1,2] | P[2,1])" could be converted to:
 * "(!B[1,1] P[1,2] P[2,1]) (!P[1,2] B[1,1]) (!P[2,1] B[1,1])"
 * 
 * Usage example:
 * 
 * // Convert logic expression into CNF clauses
 * String cnfSentence = CNFconvert.getCNF(expressionSentence);
 * 
 * Atom is compatible with any character as long as it's not operator or white
 * space.
 * 
 * Converting process is following:
 * 1. Convert infix sentence to postfix sentence.
 * 2. Use postfix sentence build a expression tree.
 * 3. Convert(eliminate) all the biconditionals in tree.
 * 4. Convert(eliminate) all the implication in tree.
 * 5. Use Morgan's rule and Double-negation elimination to convert all negation.
 * 6. Use disjuntion distribution role to separate all conjunctions to upper
 * levels and all disjunctions to lower levels.
 * 7. Make the converted CNF tree to string.
 */

public class CNFconvert {
    public static final String getCNF (final String infix) {
        // Setup all the logic operators
        final ExpressionAnalyzer expression = getExpAnalyzer();
        // Setup sentence to convert
        expression.setExpression(infix);
        // Get postfix of the sentence
        final String postfix = Postfix.getPostfixbyInfix(expression);
        // Resetup the sentence for building expression tree
        expression.setExpression(postfix);
        // Get a raw expression tree
        BTree tree = ExpressionTree.getExpressionTree(expression);
        // Eliminate all biconditionals
        tree = bicondConvert(tree);
        // Eliminate all implications
        tree = implicConvert(tree);
        /* Eliminate all double-negations and move all negations stick to the
         * operands */
        tree = morganConvert(tree);
        /* Separate all conjunctions to upper levels and all disjunctions to
         * lower levels */
        tree = distriOfDisjunct(tree);
        // Now we've got a CNF tree, transfer it into String
        return cnfTreeToString(tree);
    }

    private static ExpressionAnalyzer getExpAnalyzer () {
        final ExpressionAnalyzer expression = new ExpressionAnalyzer();
        // & | => <=> has the same priority
        expression.addOperator("&", ExpressionAnalyzer.MIN_PRI + 1, 2);
        expression.addOperator("|", ExpressionAnalyzer.MIN_PRI + 1, 2);
        expression.addOperator("=>", ExpressionAnalyzer.MIN_PRI + 1, 2);
        expression.addOperator("<=>", ExpressionAnalyzer.MIN_PRI + 1, 2);
        // 'NOT' have the higher priority, and it's an unary operator
        expression.addOperator("!", ExpressionAnalyzer.MIN_PRI + 2, 1);
        return expression;
    }

    /* Tree Convert Methods Begin ************************************** */
    // a <=> b to (a => b) & (b => a)
    private static BTree bicondConvert (final BTree tree) {
        // Use Post order Traversal: leftChild, rightChild, node
        if (tree == null) {
            return null;
        }
        final BTree lChild = bicondConvert(tree.leftChild);
        final BTree rChild = bicondConvert(tree.rightChild);

        final BTree newTree;
        if (tree.data.equals("<=>")) {
            /* Here lChild or rChild should clone a new one. Otherwise it will
             * point to the same thing. */
            final BTree newLeftChild =
                    new BTree("=>", lChild.clone(), rChild.clone());
            final BTree newRightChild =
                    new BTree("=>", rChild.clone(), lChild.clone());
            newTree = new BTree("&", newLeftChild, newRightChild);
        } else {
            /* It's not <=>, so do nothing with data. But the children might
             * have been changed, so return a new tree. */
            newTree = new BTree(tree.data, lChild, rChild);
        }
        return newTree;
    }

    // a => b to !a | b
    private static BTree implicConvert (final BTree tree) {
        // Use Post order Traversal: leftChild, rightChild, node
        if (tree == null) {
            return null;
        }
        final BTree lChild = implicConvert(tree.leftChild);
        final BTree rChild = implicConvert(tree.rightChild);

        final BTree newTree;
        if (tree.data.equals("=>")) {
            // ! symbol is a unary operator, only has a right child
            final BTree newLeftChild = new BTree("!", null, lChild);
            final BTree newRightChild = rChild;
            newTree = new BTree("|", newLeftChild, newRightChild);
        } else {
            /* It's not =>, so do nothing with data. But the children might
             * have been changed, so return a new tree. */
            newTree = new BTree(tree.data, lChild, rChild);
        }
        return newTree;
    }

    /* Morgan's rule 1: !(a & b) to (!a | !b)
     * Morgan's rule 2: !(a | b) to (!a & !b)
     * Double-negation elimination: !(!a) to a */
    private static BTree morganConvert (final BTree tree) {
        // Use Pre order Traversal: node, leftChild, rightChild
        if (tree == null) {
            return null;
        }

        String morganSymbol = null;
        if (tree.data.equals("!")) {
            final String childData = tree.rightChild.data;
            if (childData.equals("!")) { // Double-negation elimination
                morganSymbol = "!";
            } else if (childData.equals("&")) { // Morgan's rule 1
                morganSymbol = "|";
            } else if (childData.equals("|")) { // Morgan's rule 2
                morganSymbol = "&";
            }
        }

        final BTree newTree;
        if (morganSymbol != null) {
            // Need to apply double elimination or Morgan's rule
            final BTree tempTree;
            final BTree onlyChild = tree.rightChild;
            if (morganSymbol.equals("!")) {
                /* Double "!"s eliminate each other, so remainder is
                 * the right right child of current node. */
                tempTree = onlyChild.rightChild.clone();
            } else {
                /* Morgan's rule situation.
                 * Generate a converted node (Move "!" one depth down)¡£ */
                final BTree newLeftChild =
                        new BTree("!", null, onlyChild.leftChild.clone());
                final BTree newRightChild =
                        new BTree("!", null, onlyChild.rightChild.clone());
                /* Here the "morganSymbol" is the data had already been
                 * generated before */
                tempTree = new BTree(morganSymbol, newLeftChild, newRightChild);
            }
            // Go on checking the converted tree
            newTree = morganConvert(tempTree);
        } else {
            /* Current node is not "!", or the child of current node is not "!",
             * "&" or "|". So cannot apply any rule here. So go on checking
             * deeper level. */
            final BTree newLeftChild = morganConvert(tree.leftChild);
            final BTree newRightChild = morganConvert(tree.rightChild);
            newTree = new BTree(tree.data, newLeftChild, newRightChild);
        }
        return newTree;
    }

    /* Distribution rule: (a | (b & c)) to ((a | b) & (a | c))
     * 
     * If current tree need to be redistributed, I will generate a
     * redistributed temp tree first, and then recursively check every level of
     * temp tree.
     * 
     * If the left and right children of current tree both can apply
     * distribution rule, I will only split the nodes of left child. Because the
     * right child will be in one level deeper in the redistributed temp tree,
     * so the right child will be checked in future. */
    private static BTree distriOfDisjunct (final BTree tree) {
        // Use Pre order Traversal: node, leftChild, rightChild
        if (tree == null) {
            return null;
        }
        String splitSide = null;
        if (tree.data.equals("|")) {
            if (tree.leftChild.data.equals("&")) {
                // Means need to split left side for distributing
                splitSide = "left";
            } else if (tree.rightChild.data.equals("&")) {
                // Means need to split right side for distributing
                splitSide = "right";
            }
        }
        final BTree newTree;
        if (splitSide != null) {
            // Need to apply distribution rule
            final BTree tempTree =
                    redistribute(tree.leftChild, tree.rightChild, splitSide);
            // Go on checking the converted tree
            newTree = distriOfDisjunct(tempTree);
        } else {
            /* Current node is not |, or there is no & in under level. So cannot
             * apply distribution rule here. So go on checking deeper level. */
            final BTree newLeftChild = distriOfDisjunct(tree.leftChild);
            final BTree newRightChild = distriOfDisjunct(tree.rightChild);
            newTree = new BTree(tree.data, newLeftChild, newRightChild);
        }
        return newTree;
    }

    private static BTree redistribute (final BTree leftChild,
            final BTree rightChild, final String side) {
        final BTree splitPart1;
        final BTree splitPart2;
        final BTree unsplit;
        if (side.equals("left")) {
            // split the left child
            splitPart1 = leftChild.leftChild.clone();
            splitPart2 = leftChild.rightChild.clone();
            unsplit = rightChild.clone();
        } else {
            // split the right child
            splitPart1 = rightChild.leftChild.clone();
            splitPart2 = rightChild.rightChild.clone();
            unsplit = leftChild.clone();
        }

        // Recombine these parts
        final BTree newLeftChild;
        final BTree newRightChild;
        if (side.equals("left")) {
            // Unsplit one should be the right child
            newLeftChild = new BTree("|", splitPart1, unsplit);
            newRightChild = new BTree("|", splitPart2, unsplit);
        } else {
            // Unsplit one should be the left child
            newLeftChild = new BTree("|", unsplit, splitPart1);
            newRightChild = new BTree("|", unsplit, splitPart2);
        }
        return new BTree("&", newLeftChild, newRightChild);
    }

    /* Tree Convert Methods End ************************************** */

    /* Tree to String Methods Begin ************************************** */
    private static String cnfTreeToString (final BTree tree) {
        return conjunctTraveral(tree);
    }

    private static String conjunctTraveral (final BTree tree) {
        // Use Post order Traversal: leftChild, rightChild, node
        if (tree == null) {
            return "";
        }
        /* There are only 4 types of element in CNF tree:
         * &, |, !, operand */
        final StringBuffer sBuffer = new StringBuffer();
        final String curData = tree.data;
        if (curData.equals("&")) {
            final String leftStr = conjunctTraveral(tree.leftChild);
            final String rightStr = conjunctTraveral(tree.rightChild);
            sBuffer.append(leftStr);
            sBuffer.append(" ");
            sBuffer.append(rightStr);
        } else if (curData.equals("|")) {
            // Meet first "|", then the whole tree of its will contain no "&"s.
            final String leftStr = disjunctTraversal(tree.leftChild);
            final String rightStr = disjunctTraversal(tree.rightChild);
            sBuffer.append("(");
            sBuffer.append(leftStr);
            sBuffer.append(" ");
            sBuffer.append(rightStr);
            sBuffer.append(")");
        } else if (curData.equals("!")) {
            // The child of "!" can only be an operand
            final String operand = tree.rightChild.data;
            sBuffer.append("(");
            sBuffer.append("!");
            sBuffer.append(operand);
            sBuffer.append(")");
        } else {
            // Just operand
            sBuffer.append("(");
            sBuffer.append(curData);
            sBuffer.append(")");
        }
        return sBuffer.toString();
    }

    private static String disjunctTraversal (final BTree tree) {
        // Use Post order Traversal: leftChild, rightChild, node
        if (tree == null) {
            return "";
        }
        /* Once we meet a "|", there are only 3 types of element in CNF tree:
         * |, !, operand */
        final StringBuffer sBuffer = new StringBuffer();
        final String curData = tree.data;
        if (curData.equals("|")) {
            final String leftStr = disjunctTraversal(tree.leftChild);
            final String rightStr = disjunctTraversal(tree.rightChild);
            sBuffer.append(leftStr);
            sBuffer.append(" ");
            sBuffer.append(rightStr);
        } else if (curData.equals("!")) {
            // The child of "!" can only be an operand
            final String operand = disjunctTraversal(tree.rightChild);
            sBuffer.append("!");
            sBuffer.append(operand);
        } else {
            // Just operand
            sBuffer.append(curData);
        }
        return sBuffer.toString();
    }

    /* Tree to String Methods End ************************************** */

    /* Methods for Test Begin ************************************** */
    private static final String TEST1 =
            "(!Mythical & ((!Horned | !abc) | !abc| !abc))";
    private static final String TEST2 = "(a <=> b) <=> (c <=> d)";
    private static final String TEST3 = "a <=> b <=> c <=> d";
    private static final String TEST4 = "a <=> b";
    private static final String TEST5 = "!!a";
    private static final String TEST6 = "!(!a | !b)";
    private static final String TEST7 = "!(a <=> b)";
    private static final String TEST8 = "a |(b & c)";
    private static final String TEST9 = "(a & b)|(c & d)";
    private static final String TEST10 = "(B11 <=> (P12 | P21))";
    private static final String TEST11 = "(B21 <=> (P11 | P22 | P31))";
    private static final String TEST12 =
            "(!B21 | P11 | P22 | P31) &(!P11 | B21) & (!P22 | B21) & (!P31 | B21)";
    private static final String TEST13 = "B[1,1] <=> (P[1,2] | P[2,1])";

    public static void main (final String[] args) {
        final ExpressionAnalyzer expression = getExpAnalyzer();
        expression.setExpression(TEST13);
        System.out.println(expression);
        final String postfix = Postfix.getPostfixbyInfix(expression);
        System.out.println(postfix);
        expression.setExpression(postfix);
        BTree tree = ExpressionTree.getExpressionTree(expression);
        System.out.println(tree);

        tree = bicondConvert(tree);
        System.out.println(tree);

        tree = implicConvert(tree);
        System.out.println(tree);

        tree = morganConvert(tree);
        System.out.println(tree);

        tree = distriOfDisjunct(tree);
        System.out.println(tree);

        System.out.println(cnfTreeToString(tree));
    }
    /* Methods for Test End ************************************** */
}
