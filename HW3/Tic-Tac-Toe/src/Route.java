public class Route implements Cloneable {
    private int numOfX = 0;
    private int numOfO = 0;

    public int getNumOfPieces (int player) {
        if (player == Board.PLAYER_X) {
            return numOfX;
        } else {
            return numOfO;
        }
    }
    
    /**
     * @return: true, has winner; false, not.
     */
    public boolean addOnePiece (int player) {
        boolean hasWinner = false;
        addNumOfPieces(player);
        if (getNumOfPieces(player) == Board.WINCOUNT) {
            hasWinner = true;
        }
        return hasWinner;
    }

    public Object clone () {
        Route o = null;
        try {
            o = (Route) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        return o;
    }
    private void addNumOfPieces (int player) {
        if (player == Board.PLAYER_X) {
            numOfX++;
            assert numOfX <= Board.WINCOUNT;
        } else {
            numOfO++;
            assert numOfO <= Board.WINCOUNT;
        }
    }
}
