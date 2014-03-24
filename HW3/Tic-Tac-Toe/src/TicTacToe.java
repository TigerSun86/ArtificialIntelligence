import java.util.Scanner;

public class TicTacToe {
    private static final int PLAYER_TYPE_HUMAN = 0;
    private static final int PLAYER_TYPE_AI = 1;
    private static final int AI_DEPTH_INITIAL = 1;
    private static final int AI_DEPTH_STEP = 2;
    private static int[] typeOfPlayer = new int[Board.PLAYER_O + 1];
    private static int[] maxDepthOfAI = new int[Board.PLAYER_O + 1];
    private static int[] depthOfAI = new int[Board.PLAYER_O + 1];

    /* display begin ***************************************************** */
    private static final String SMALL_EMPTY_SPACE = " ";
    private static final String LARGE_EMPTY_SPACE = "  ";
    private static final String AHEAD_SPACE = LARGE_EMPTY_SPACE;
    private static final char[] CHAR_PIECES = { ' ', 'X', 'O' };

    private static void printDashLine () {
        // display dash line
        System.out.printf("%s", AHEAD_SPACE);
        for (int levelID = 0; levelID < Board.MAX_LEVEL; levelID++) {
            System.out.printf("-");
            for (int columnID = 0; columnID < Board.MAX_COLUMN; columnID++) {
                System.out.printf("--");
            }
            System.out.printf("%s", LARGE_EMPTY_SPACE);
        }
        System.out.println();
    }

    private static void displayGameBoard (BoardGame game) {
        System.out.println();
        // display level label
        System.out.printf("%s", AHEAD_SPACE);
        for (int levelID = 0; levelID < Board.MAX_LEVEL; levelID++) {
            System.out.printf("%sLevel %d%s%s", SMALL_EMPTY_SPACE, levelID,
                    SMALL_EMPTY_SPACE, LARGE_EMPTY_SPACE);
        }
        System.out.println();
        System.out.println();

        // display column label
        System.out.printf("%s", AHEAD_SPACE);
        for (int levelID = 0; levelID < Board.MAX_LEVEL; levelID++) {
            System.out.printf("%s", SMALL_EMPTY_SPACE);
            for (int columnID = 0; columnID < Board.MAX_COLUMN; columnID++) {
                System.out.printf("%d%s", columnID, SMALL_EMPTY_SPACE);
            }
            System.out.printf("%s", LARGE_EMPTY_SPACE);
        }
        System.out.println();
        printDashLine();

        // display board content
        for (int rowID = 0; rowID < Board.MAX_ROW; rowID++) {
            System.out.printf("%d ", rowID);
            for (int levelID = 0; levelID < Board.MAX_LEVEL; levelID++) {
                System.out.printf("|");
                for (int columnID = 0; columnID < Board.MAX_COLUMN; columnID++) {
                    final int currentSpot =
                            game.getSpotValue(levelID, rowID, columnID);
                    System.out.printf("%C|", CHAR_PIECES[currentSpot]);
                }
                System.out.printf("%s", LARGE_EMPTY_SPACE);
            }
            System.out.println();
            printDashLine();
        }
    }

    /* display end ***************************************************** */
    private static boolean isLegalInput (String[] oneMove) {
        boolean isLegal = true;
        if (oneMove.length != 3) {
            isLegal = false;
        } else { // if (oneMove.length == 3) {
            try {
                Integer.valueOf(oneMove[0]);
                Integer.valueOf(oneMove[1]);
                Integer.valueOf(oneMove[2]);
            } catch (final NumberFormatException e) {
                isLegal = false;
            }
        }
        return isLegal;
    }

    private static void setAIDifficulty (int player, Scanner scanner) {
        boolean needInput = true;
        while (needInput) {
            System.out.printf(
                    "Please input the depth of thought of AI player %C:%n",
                    CHAR_PIECES[player]);

            final String strDepth = scanner.nextLine();
            try {
                final int depth = Integer.valueOf(strDepth);
                maxDepthOfAI[player] = depth;
                depthOfAI[player] = AI_DEPTH_INITIAL;
                needInput = false;
            } catch (final NumberFormatException e) { // illegal input, reinput
                System.out.println("Input illegal.");
                needInput = true;
            }
        }
    }

    private static void setPlayerType (int player, Scanner scanner) {
        boolean needInput = true;
        while (needInput) {
            System.out.printf(
                    "Please input the typy of player %C (Human / AI):%n",
                    CHAR_PIECES[player]);

            final String playerType = scanner.nextLine();
            if (playerType.charAt(0) == 'h' || playerType.charAt(0) == 'H') {
                typeOfPlayer[player] = PLAYER_TYPE_HUMAN;
                needInput = false;
            } else if (playerType.charAt(0) == 'a'
                    || playerType.charAt(0) == 'A') {
                typeOfPlayer[player] = PLAYER_TYPE_AI;
                setAIDifficulty(player, scanner);
                needInput = false;
            } else { // illegal input, reinput
                System.out.println("Input illegal.");
            }
        }
    }

    private static Spot getAIMove (BoardGame game, Scanner scanner) {
        final Spot newMove;
        final int currentPlayer = game.getCurrentPlayer();
        System.out.printf("%nPlayer %C is thinking...%n",
                CHAR_PIECES[currentPlayer]);
        newMove =
                AlphaBetaPruning.alphaBetaDecision(game,
                        game.getCurrentPlayer(), depthOfAI[currentPlayer]);
        /* at start, do not think too complexly; as time goes, think more
         * and more deeply */
        depthOfAI[currentPlayer] =
                Math.min((depthOfAI[currentPlayer] + AI_DEPTH_STEP),
                        maxDepthOfAI[currentPlayer]);
        System.out.printf("Player %C made a move: %d %d %d%n",
                CHAR_PIECES[currentPlayer], newMove.level, newMove.row,
                newMove.column);
        return newMove;
    }

    private static Spot getHumanMove (BoardGame game, Scanner scanner) {
        Spot newMove = new Spot();

        boolean needInput = true;
        while (needInput) {
            System.out
                    .printf("It's player %C's turn, please make a move (LevelID RowID ColumnID):%n",
                            CHAR_PIECES[game.getCurrentPlayer()]);
            final String[] oneMove = scanner.nextLine().split(" ");
            // input string legal test
            if (isLegalInput(oneMove)) {
                newMove.level = Integer.valueOf(oneMove[0]);
                newMove.row = Integer.valueOf(oneMove[1]);
                newMove.column = Integer.valueOf(oneMove[2]);
                // input spot legal test
                if (game.isLegalMove(newMove.level, newMove.row, newMove.column)) {
                    needInput = false;
                }
            }

            if (needInput) {
                System.out.println("Input illegal.");
            }
        } // while (needInput) {
        return newMove;
    }

    private static boolean MakeOneMove (BoardGame game, Scanner scanner) {
        // get one move
        final Spot newMove;
        if (typeOfPlayer[game.getCurrentPlayer()] == PLAYER_TYPE_AI) {
            /* call AI */
            newMove = getAIMove(game, scanner);
        } else { // if (currentPlayer == PLAYER_TYPE_HUMAN)
            newMove = getHumanMove(game, scanner);
        }

        // make the move on the board
        game.makeMove(newMove.level, newMove.row, newMove.column);

        displayGameBoard(game);

        if (game.getWinner() != Board.PLAYER_EMPTY) {
            System.out.printf("Winner is player %C%n",
                    CHAR_PIECES[game.getWinner()]);
            return true;
        } else if (game.isFull()) {
            System.out.printf("It's a tie%n");
            return true;
        } else {
            return false;
        }
    }

    private static void startOnePlay () {
        final Scanner scanner = new Scanner(System.in, "US-ASCII");
        // set player X type
        setPlayerType(Board.PLAYER_X, scanner);
        // set player O type
        setPlayerType(Board.PLAYER_O, scanner);
        BoardGame game = new BoardGame();
        displayGameBoard(game);
        boolean isEnd = false;
        while (!isEnd) {
            isEnd = MakeOneMove(game, scanner);
        }

    }

    public static void main (String args[]) {
        startOnePlay();
    }
}
