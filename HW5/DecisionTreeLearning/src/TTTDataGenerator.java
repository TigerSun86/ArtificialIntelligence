import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Random;

public class TTTDataGenerator {
    public static void main (final String[] args) {
        System.out.println("Start generating...");
        genTTTMoveData(200, "ttt-move-train.txt", "ttt-move-train2.txt", 50,
                "ttt-move-test.txt", "ttt-move-test2.txt");
        System.out.println("Done.");
    }

    public static void
            genTTTMoveData (final int trainSize, final String trainFName,
                    final String train2FName, final int testSize,
                    final String testFName, final String test2FName) {
        genOneFile(trainSize, trainFName, false);
        genOneFile(testSize, testFName, false);
        // Additional attribute.
        genOneFile(trainSize, train2FName, true);
        genOneFile(testSize, test2FName, true);
    }

    private static void genOneFile (final int size, final String fName,
            final boolean needBlankNum) {
        final HashSet<Example> trainSet = genSet(size, needBlankNum);
        writefile(trainSet, fName);
    }

    private static void writefile (final HashSet<Example> dataSet,
            final String fName) {
        try {
            File file = new File(fName);

            // If file doesn't exists, then create it.
            if (!file.exists()) {
                file.createNewFile();
            }

            FileWriter fw = new FileWriter(file.getAbsoluteFile());
            BufferedWriter bw = new BufferedWriter(fw);
            for (Example example : dataSet) {
                bw.write(example.toString());
                bw.newLine();
            }

            bw.close();
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    private static HashSet<Example> genSet (final int size,
            final boolean needBlankNum) {
        final HashSet<Example> dataSet = new HashSet<Example>();
        while (dataSet.size() < size) {
            final Example e = genOne(needBlankNum);
            assert e != null;
            // Redundant examples have same hashCode, so wouldn't count.
            dataSet.add(e);
        }
        return dataSet;
    }

    private static final String X_PIECE = "x";
    private static final String O_PIECE = "o";
    private static final String B_PIECE = "b";

    private static Example genOne (final boolean needBlankNum) {
        Example example = null;
        final Random random = new Random(System.currentTimeMillis());
        final String[][] board = new String[3][3];
        String curPlayer = X_PIECE;
        int count = 0;
        while (true) {
            // Make a move.
            final int row = random.nextInt(3);
            final int col = random.nextInt(3);
            if (board[row][col] == null) {
                // Valid move.
                board[row][col] = curPlayer;
                // Switch player.
                if (curPlayer.equals(X_PIECE)) {
                    curPlayer = O_PIECE;
                } else {
                    curPlayer = X_PIECE;
                }
                count++;
                if (count >= 5) {
                    // At least 5 moves can end a game.
                    example = endTest(board, count, needBlankNum);
                    if (example != null) {
                        // Game over.
                        break;
                    }
                }
            } // if (board[row][col] == null) {
        } // while (true) {
        return example;
    }

    private static Example endTest (final String[][] board, final int count,
            final boolean needBlankNum) {
        final Example example = new Example();
        final String winner = winner(board);
        if (winner != null) {
            // Got a winner, game over.
            example.addAll(BoardToList(board));
            if (needBlankNum) {
                example.add(Integer.toString(9 - count));
            }
            final String classifi;
            if (winner.equals(X_PIECE)) {
                classifi = "win";
            } else {
                classifi = "lose";
            }
            example.add(classifi);
        } else {
            if (count == 9) {
                // Board is full, game tie.
                example.addAll(BoardToList(board));
                if (needBlankNum) {
                    example.add(Integer.toString(9 - count));
                }
                example.add("tie");
            }
        } // if (winner != null) {

        if (!example.isEmpty()) {
            return example;
        } else {
            return null;
        }
    }

    private static ArrayList<Object> BoardToList (final String[][] board) {
        final ArrayList<Object> list = new ArrayList<Object>();
        for (int row = 0; row < 3; row++) {
            for (int col = 0; col < 3; col++) {
                if (board[row][col] != null) {
                    list.add(board[row][col]);
                } else {
                    list.add(B_PIECE);
                }
            }
        }
        return list;
    }

    private static String winner (final String[][] board) {
        // Check rows
        for (int row = 0; row < 3; row++) {
            final String winner = checkRow(board, row);
            if (winner != null) {
                return winner;
            }
        }
        // Check cols
        for (int col = 0; col < 3; col++) {
            final String winner = checkCol(board, col);
            if (winner != null) {
                return winner;
            }
        }
        // Check diags
        final String winner = checkDiag(board);
        if (winner != null) {
            return winner;
        }
        return null;
    }

    private static String checkRow (final String[][] board, final int row) {
        String winner = null;
        for (int col = 0; col < 3; col++) {
            if (board[row][col] == null) {
                return null;
            } else {
                if (winner == null) {
                    winner = board[row][col];
                } else {
                    if (!winner.equals(board[row][col])) {
                        return null;
                    }
                }
            }
        }
        return winner;
    }

    private static String checkCol (final String[][] board, final int col) {
        String winner = null;
        for (int row = 0; row < 3; row++) {
            if (board[row][col] == null) {
                return null;
            } else {
                if (winner == null) {
                    winner = board[row][col];
                } else {
                    if (!winner.equals(board[row][col])) {
                        return null;
                    }
                }
            }
        }
        return winner;
    }

    private static String checkDiag (final String[][] board) {
        String winner = null;
        for (int row = 0, col = 0; row < 3 && col < 3; row++, col++) {
            if (board[row][col] == null) {
                return null;
            } else {
                if (winner == null) {
                    winner = board[row][col];
                } else {
                    if (!winner.equals(board[row][col])) {
                        return null;
                    }
                }
            }
        }
        if (winner != null) {
            return winner;
        }

        for (int row = 0, col = 2; row < 3 && col >= 0; row++, col--) {
            if (board[row][col] == null) {
                return null;
            } else {
                if (winner == null) {
                    winner = board[row][col];
                } else {
                    if (!winner.equals(board[row][col])) {
                        return null;
                    }
                }
            }
        }
        return winner;
    }

}
