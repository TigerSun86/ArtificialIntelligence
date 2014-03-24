import java.util.Arrays;

public class Board implements Cloneable {
    public static final int MAX_LEVEL = 4;
    public static final int MAX_ROW = 4;
    public static final int MAX_COLUMN = 4;
    public static final int WINCOUNT = 4;

    public static final int PLAYER_EMPTY = 0;
    public static final int PLAYER_X = 1;
    public static final int PLAYER_O = 2;
    private static final int MAX_PIECES_NUMBER = MAX_LEVEL * MAX_ROW
            * MAX_COLUMN;

    private int piecesNumber = 0;
    private int[][][] boardArray = new int[MAX_LEVEL][MAX_ROW][MAX_COLUMN];

    private void initialBoard () {
        for (int levelID = 0; levelID < MAX_LEVEL; levelID++) {
            for (int rowID = 0; rowID < MAX_ROW; rowID++) {
                Arrays.fill(boardArray[levelID][rowID], PLAYER_EMPTY);
            }
        }
    }

    public Board() {
        initialBoard();
    }

    public void makeMove (int levelID, int rowID, int columnID, int player) {
        boardArray[levelID][rowID][columnID] = player;
        piecesNumber++;
    }

    public int getSpotValue (int levelID, int rowID, int columnID) {
        return boardArray[levelID][rowID][columnID];
    }

    public boolean isLegalMove (int levelID, int rowID, int columnID) {
        boolean isLegal = true;
        // check whether the move is out of bound
        if (levelID >= MAX_LEVEL || levelID < 0) {
            isLegal = false;
        } else if (rowID >= MAX_ROW || rowID < 0) {
            isLegal = false;
        } else if (columnID >= MAX_COLUMN || columnID < 0) {
            isLegal = false;
        }

        if (isLegal) {
            // check whether the move is in the spot has been occupied
            if (getSpotValue(levelID, rowID, columnID) != PLAYER_EMPTY) {
                isLegal = false;
            }
        }
        return isLegal;
    }

    public boolean isFull () {
        return (piecesNumber == MAX_PIECES_NUMBER);
    }

    public static int getOpponent (int player) {

        if (player == PLAYER_X) {
            return PLAYER_O;
        } else {
            return PLAYER_X;
        }
    }

    public Object clone () {
        Board o = null;
        try {
            o = (Board) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        o.boardArray = new int[MAX_LEVEL][MAX_ROW][MAX_COLUMN];
        for (int levelID = 0; levelID < MAX_LEVEL; levelID++) {
            for (int rowID = 0; rowID < MAX_ROW; rowID++) {
                o.boardArray[levelID][rowID] =
                        Arrays.copyOf(boardArray[levelID][rowID],
                                boardArray[levelID][rowID].length);
            }
        }

        return o;
    }
}
