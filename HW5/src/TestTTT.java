public class TestTTT {
    private static final String ATTR_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/ttt-attr.txt";
    private static final String TRAIN_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/ttt-train.txt";
    private static final String TEST_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/ttt-test.txt";

    public static void main (final String[] args) {
        DecisionTreeLearning.testDecisionTree(ATTR_FILE_URL, TRAIN_FILE_URL,
                TEST_FILE_URL);
    }
}
