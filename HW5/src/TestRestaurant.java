public class TestRestaurant {
    // private static final String ATTR_FILE_URL =
    // "file:///C:/WorkSpace/Artificial Intelligence/HW5/restaurant-attr.txt";
    // private static final String TRAIN_FILE_URL =
    // "file:///C:/WorkSpace/Artificial Intelligence/HW5/restaurant-train.txt";
    // private static final String TEST_FILE_URL = null;
    private static final String ATTR_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/restaurant-attr.txt";
    private static final String TRAIN_FILE_URL =
            "http://cs.fit.edu/~pkc/classes/ai/ml/restaurant-train.txt";
    private static final String TEST_FILE_URL = null;

    public static void main (final String[] args) {
        DecisionTreeLearning.testDecisionTree(ATTR_FILE_URL, TRAIN_FILE_URL,
                TEST_FILE_URL);
    }
}
