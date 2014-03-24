import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

public class AlphaBetaPruning {
    private static final int ORDER_ASCENDING = 0;
    private static final int ORDER_DESCENDING = 1;
    private static final double DECENT_LOSE_FACTOR = -0.9;

    public static Spot alphaBetaDecision (BoardGame game, int myPlayer, int depth) {
        // get all next possible moves
        TreeMap<Integer, ArrayList<BoardGame>> queue =
                generateChildStates(game, myPlayer, ORDER_DESCENDING);
        
        int utilityMax = Integer.MIN_VALUE;
        Spot bestMove = null;
        for (Map.Entry<Integer, ArrayList<BoardGame>> entry : queue.entrySet()) {
            final ArrayList<BoardGame> childrenList = entry.getValue();
            for (int i = 0; i < childrenList.size(); i++) {
                // get one child
                final BoardGame child = childrenList.get(i);
                final int utility = minValue(child, myPlayer, utilityMax, Integer.MAX_VALUE, depth - 1);
                if (utilityMax < utility) {
                    utilityMax = utility;
                    bestMove = child.getLastMove();
                }
            } // for (int i = 0; i < childrenList.size(); i++) {
        } // for (Map.Entry<Integer, ArrayList<BoardGame>>

        if (utilityMax < (int) (WinRoutes.INFINITE_UTILITY * DECENT_LOSE_FACTOR)) {
            // if I must die, fight until the end
            System.out.println("I am a warrior.");
            final BoardGame decentGame = queue.firstEntry().getValue().get(0);
            bestMove = decentGame.getLastMove();
        }
        return bestMove;
    }

    private static int maxValue (BoardGame game, int myPlayer, int alphaOfParent,
            int betaOfParent, int depth) {
        if (isTerminal(game, depth)) {
            return game.getUtility(myPlayer);
        }
        // get all next possible moves
        TreeMap<Integer, ArrayList<BoardGame>> queue =
                generateChildStates(game, myPlayer, ORDER_DESCENDING);

        int alphaOfMine = alphaOfParent;
        boolean breakFlag = false;
        for (Map.Entry<Integer, ArrayList<BoardGame>> entry : queue.entrySet()) {
            final ArrayList<BoardGame> childrenList = entry.getValue();
            for (int i = 0; i < childrenList.size(); i++) {
                // get one child
                final BoardGame child = childrenList.get(i);
                alphaOfMine =
                        Math.max(
                                alphaOfMine,
                                minValue(child, myPlayer, alphaOfMine,
                                        betaOfParent, depth - 1));
                if (alphaOfMine >= betaOfParent) {
                    // prune all siblings after this
                    breakFlag = true;
                    break;
                }
            } // for (int i = 0; i < childrenList.size(); i++) {
            if (breakFlag) {
                break;
            }
        } // for (Map.Entry<Integer, ArrayList<BoardGame>>
        return alphaOfMine;
    }

    private static int minValue (BoardGame game, int myPlayer, int alphaOfParent,
            int betaOfParent, int depth) {
        if (isTerminal(game, depth)) {
            return game.getUtility(myPlayer);
        }
        // get all next possible moves
        TreeMap<Integer, ArrayList<BoardGame>> queue =
                generateChildStates(game, myPlayer, ORDER_ASCENDING);

        int betaOfMine = betaOfParent;
        boolean breakFlag = false;
        for (Map.Entry<Integer, ArrayList<BoardGame>> entry : queue.entrySet()) {
            final ArrayList<BoardGame> childrenList = entry.getValue();
            for (int i = 0; i < childrenList.size(); i++) {
                // get one child
                final BoardGame child = childrenList.get(i);
                betaOfMine =
                        Math.min(
                                betaOfMine,
                                maxValue(child, myPlayer, alphaOfParent,
                                        betaOfMine, depth - 1));
                if (betaOfMine <= alphaOfParent) {
                    // prune all siblings after this
                    breakFlag = true;
                    break;
                }
            } // for (int i = 0; i < childrenList.size(); i++) {
            if (breakFlag) {
                break;
            }
        } // for (Map.Entry<Integer, ArrayList<BoardGame>>
        return betaOfMine;
    }

    private static TreeMap<Integer, ArrayList<BoardGame>> generateChildStates (
            BoardGame game, int myPlayer, int order) {
        TreeMap<Integer, ArrayList<BoardGame>> queue;
        if (order == ORDER_ASCENDING) {
            queue = new TreeMap<Integer, ArrayList<BoardGame>>();
        } else { // if (order == ORDER_DESCENDING) {
            queue =
                    new TreeMap<Integer, ArrayList<BoardGame>>(
                            Collections.reverseOrder());
        }

        // get all actions
        for (int rowID = 0; rowID < Board.MAX_ROW; rowID++) {
            for (int levelID = 0; levelID < Board.MAX_LEVEL; levelID++) {
                for (int columnID = 0; columnID < Board.MAX_COLUMN; columnID++) {
                    // make a move in empty spot
                    if (game.getSpotValue(levelID, rowID, columnID) == Board.PLAYER_EMPTY) {
                        BoardGame child = (BoardGame) game.clone();
                        /* in maxValue, it's my move; in minValue, it's
                         * opponent's move. */
                        child.makeMove(levelID, rowID, columnID);
                        final int utility = child.getUtility(myPlayer);
                        final ArrayList<BoardGame> childrenList;
                        if (queue.get(utility) == null) {
                            /* to avoid identical key problem, so store child in
                             * list */
                            childrenList = new ArrayList<BoardGame>();
                        } else {
                            childrenList = queue.get(utility);
                        }
                        childrenList.add(child);
                        // store children in queue
                        queue.put(utility, childrenList);
                    }
                } // for (int columnID = 0; columnID < Board.MAX_COLUMN;
            } // for (int levelID = 0; levelID < Board.MAX_LEVEL; levelID++) {
        } // for (int rowID = 0; rowID < Board.MAX_ROW; rowID++) {
        return queue;
    }

    private static boolean isTerminal (BoardGame game, int depth) {
        if (game.getWinner() != Board.PLAYER_EMPTY) {
            return true;
        } else if (game.isFull()) {
            return true;
        } else if (depth == 0) {
            return true;
        } else {
            return false;
        }
    }
}
