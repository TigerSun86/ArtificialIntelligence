import java.util.ArrayList;
import java.util.HashMap;

public class DecisionTreeLearning {
    public static final DecisionTree learnDecisionTree (
            final ExampleSet examples, final AttributeList attributes,
            final Object defaultClass) {
        if (examples.isEmpty()) {
            return new DecisionTree(defaultClass);
        }
        final Object classification = examples.sameClassification();
        if (classification != null) {
            return new DecisionTree(classification);
        }
        if (attributes.isAllDisabled()) {
            return new DecisionTree(examples.mode());
        }
        // Choose the best attribute for the tree constructing.
        final Attribute best = chooseAttribute(examples, attributes);
        // Delete the best attribute from the attribute list,
        // so attributes becomes attributes - best.
        attributes.disable(best);
        // Construct a decision tree with root is the attribute best.
        final DecisionTree tree = new DecisionTree(best.getName());
        for (Object value : best.valueList()) {
            // Get sub example list with best == valueI.
            final ExampleSet subExamples = examples.subExampleSet(best, value);
            // Construct a sub tree.
            final DecisionTree subTree =
                    learnDecisionTree(subExamples, attributes, examples.mode());
            // Add the valueI and sub tree as a branch to the decision tree.
            tree.addBranch(value, subTree);
        }
        // Recover the deleted attribute in the attribute list before return.
        attributes.enable(best);
        return tree;
    }

    public static final void printDecisionTree (final DecisionTree tree) {
        System.out.println(tree);
    }

    public static final double evalDecisionTree (final DecisionTree tree,
            final ExampleSet testSet, final AttributeList attributes) {
        int hit = 0;
        for (Example example : testSet.getExampleSet()) {
            DecisionTree subTree = tree;
            // Hit the leaf of decision tree according to the example.
            while (!subTree.isLeaf()) {
                // Get current attribute from tree.
                final Object attrName = subTree.getRoot();
                // Get value of the attribute from test example
                final int attrIndex =
                        attributes.indexOf(attributes.get(attrName));
                final Object value = example.get(attrIndex);
                // Get the sub tree of the tree according to the value.
                subTree = subTree.getSubTree(value);
            }
            // The sub tree is a leaf.
            final Object classOfTree = subTree.getRoot();
            final Object classOfExam = example.get(attributes.indexOfTarget());
            if (classOfTree.equals(classOfExam)) {
                hit++;
            }
        }
        final int sum = testSet.size();
        return ((double) hit) / sum;
    }

    public static final void testDecisionTree (final String attrFName,
            final String trainFName, final String testFName) {
        // Initialize the data set.
        final AttributeList attrList = getAttributeList(attrFName);
        if (attrList == null) {
            // Couldn't get attributes.
            return;
        }
        final ExampleSet trainSet = getExampleSet(trainFName, attrList);
        if (trainSet == null) {
            // Couldn't get train set.
            return;
        }

        ExampleSet testSet = null;
        if (testFName != null) {
            testSet = getExampleSet(testFName, attrList);
        }

        final DecisionTree dTree =
                learnDecisionTree(trainSet, attrList, trainSet.mode());

        printDecisionTree(dTree);

        final double accuOfTrainSet =
                evalDecisionTree(dTree, trainSet, attrList);
        System.out.println(accuOfTrainSet);

        if (testSet != null) {
            final double accuOfTestSet =
                    evalDecisionTree(dTree, testSet, attrList);
            System.out.println(accuOfTestSet);
        }
    }

    private static AttributeList getAttributeList (final String attrFName) {
        final AttributeList attrList = new AttributeList();
        final DataExtractor in = new DataExtractor(attrFName);

        while (true) {
            final String[] attrStr = in.nextLine();
            if (attrStr == null) {
                break;
            }

            final Attribute attr = new Attribute(attrStr[0]);
            for (int i = 1; i < attrStr.length; i++) {
                attr.add(attrStr[i]);
            }
            attrList.add(attr);
        }

        in.close();

        if (attrList.size() <= 1) {
            System.err.println("No enough attributes in: " + attrFName);
            return null;
        } else {
            return attrList;
        }
    }

    private static ExampleSet getExampleSet (final String examFName,
            final AttributeList attrList) {
        final ExampleSet exampleSet = new ExampleSet(attrList);
        final DataExtractor in = new DataExtractor(examFName);

        while (true) {
            final String[] examStr = in.nextLine();
            if (examStr == null) {
                break;
            }
            final Example example = new Example();
            for (String str : examStr) {
                example.add(str);
            }

            if (example.size() != attrList.size()) {
                System.err
                        .println("Number of values is inconsistent with number of attributes: "
                                + example);
                // Just don't add this line of example.
                continue;
            }
            exampleSet.add(example);
        }

        in.close();

        if (exampleSet.isEmpty()) {
            System.err.println("No example in: " + examFName);
            return null;
        } else {
            return exampleSet;
        }
    }

    private static Attribute chooseAttribute (final ExampleSet examples,
            final AttributeList attributes) {
        final double before = before(examples, attributes);

        double maxGain = Double.NEGATIVE_INFINITY;
        Attribute bestAttr = null;
        for (Attribute attr : attributes.validList()) {
            final double gain = before - after(examples, attributes, attr);
            if (Double.compare(maxGain, gain) < 0) {
                maxGain = gain;
                bestAttr = attr;
            }
        }
        assert bestAttr != null;
        return bestAttr;
    }

    private static double before (final ExampleSet examples,
            final AttributeList attributes) {
        // "Before" only calculates one information uncertainty,
        // so the weight factor is 1.
        return weightedUncertainty(examples, attributes, null, null);
    }

    private static double after (final ExampleSet examples,
            final AttributeList attributes, final Attribute attr) {
        double weightedInfoUncerSum = 0;
        for (Object value : attr.valueList()) {
            weightedInfoUncerSum +=
                    weightedUncertainty(examples, attributes, attr, value);
        }
        return weightedInfoUncerSum;
    }

    private static double weightedUncertainty (final ExampleSet examples,
            final AttributeList attributes, final Attribute attr,
            final Object value) {
        final int sum = examples.numberOf(attr, value, null);
        if (sum == 0) {
            // If there's no example of this value, the uncertainty is 0.
            return 0;
        }

        final ArrayList<Double> probs = new ArrayList<Double>();
        for (Object classifi : attributes.classList()) {
            // Get number of examples filtered by class.
            final int number = examples.numberOf(attr, value, classifi);
            // Calculate the probability of this class.
            probs.add(((double) number) / sum);
        }
        final int universalSet = examples.numberOf(null, null, null);
        final double factor = ((double) sum) / universalSet;
        return infoUncertainty(probs) * factor;
    }

    private static final double LOG_2 = Math.log(2);
    // To make log calculation faster.
    private static final HashMap<Double, Double> LOG_CACHE =
            new HashMap<Double, Double>();

    private static double
            infoUncertainty (final ArrayList<Double> probabilities) {
        double sum = 0;
        for (double prob : probabilities) {
            if (prob == 0) {
                // If prob == 0, the logProb will be neg infinite, and this part
                // of uncertainty is 0.
                continue;
            }

            Double logP = LOG_CACHE.get(prob);
            if (logP == null) {
                logP = Math.log(prob + Double.MIN_VALUE);
                LOG_CACHE.put(prob, logP);
            }
            // I(P(v1),...P(vn))= sum(−P(vi) * log2 P(vi))
            sum += -prob * (logP / LOG_2);
        }
        return sum;
    }
}
