
public class TestIDS {
    private static final String ATTR_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/ids-attr.txt";
    private static final String TRAIN_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/ids-train.txt";
    private static final String TEST_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/ids-test.txt";

    public static void main (final String[] args) {
        DecisionTreeLearning.testDecisionTree(ATTR_FILE_URL, TRAIN_FILE_URL,
                TEST_FILE_URL);
    }
}
