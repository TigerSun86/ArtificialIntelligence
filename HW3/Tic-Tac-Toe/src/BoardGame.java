public class BoardGame implements Cloneable {
    private Board board = new Board();
    private WinRoutes winRoutes = new WinRoutes();
    private int currentPlayer;
    private Spot lastMove = new Spot();

    /* public method begin ******************************** */
    public Spot getLastMove() {
        return lastMove;
    }
    
    public BoardGame() {
        currentPlayer = Board.PLAYER_X;
    }

    public int getCurrentPlayer () {
        return currentPlayer;
    }

    public void makeMove (int levelID, int rowID, int columnID) {
        board.makeMove(levelID, rowID, columnID, currentPlayer);
        updateLastMove(levelID, rowID, columnID);
        winRoutes.update(levelID, rowID, columnID);
        switchPlayer();
    }

    public int getSpotValue (int levelID, int rowID, int columnID) {
        return board.getSpotValue(levelID, rowID, columnID);
    }

    public boolean isLegalMove (int levelID, int rowID, int columnID) {
        return board.isLegalMove(levelID, rowID, columnID);
    }
    public boolean isFull () {
        return board.isFull();
    }
    public int getUtility (int player) {
        return winRoutes.getUtility(player);
    }

    public int getWinner () {
        return winRoutes.getWinner();
    }

    public Object clone () {
        BoardGame o = null;
        try {
            o = (BoardGame) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        o.board = (Board) board.clone();
        o.winRoutes = (WinRoutes) winRoutes.clone();
        o.lastMove = (Spot) lastMove.clone();
        return o;
    }

    /* public method end ******************************** */
    private void updateLastMove (int levelID, int rowID, int columnID) {
        lastMove.level = levelID;
        lastMove.row = rowID;
        lastMove.column = columnID;
    }
    private void switchPlayer () {
        if (currentPlayer == Board.PLAYER_X) {
            currentPlayer = Board.PLAYER_O;
        } else {
            currentPlayer = Board.PLAYER_X;
        }
    }
}
