/**
 * routeID: one integer
 * last 2 bits represent 2nd index;
 * next 2 bits represent 1st index;
 * next 3 bits represent route type id.
 */
/**
 * routeTypeID: represent different kinds of possible win route
 * straight:
 * 0, level changes, other remain
 * 1, row changes, other remain
 * 2, column changes, other remain
 * 
 * diagonal:
 * 3, level remains, others change
 * 4, row remains, others change
 * 5, column remains, others change
 * in 3,4,5, there are 2 kinds of relationship of others: direct ratio or
 * inverse ratio.
 * 
 * 6, all change
 * in 6, there are 4 kinds of relationship of all:
 * 0, all are direct ratio; 1, level is inverse ratio, others are direct ratio;
 * 2, row is inverse ratio, others are direct ratio; 3, column is inverse ratio,
 * others are direct ratio
 */
public class WinRoutes implements Cloneable {
    private static final int MAX_ROUTE_TYPE_ID = 6;
    private static final int MAX_FIRST_INDEX = 3;
    private static final int MAX_SECOND_INDEX = 3;
    private static final int ROUTE_TYPE_DIRECT_RATIO = 0;
    private static final int ROUTE_TYPE_INVERSE_RATIO = 1;
    private static final int TURN_ATTACK = 0; // it's your turn
    private static final int TURN_DEFFENCE = 1; // it's opponent's turn
    /* evaluation system begin ****** */
    private static final int OFFENSIVE_FACTOR = 10;
    private static final int SUPERIORITY_FACTOR = 10;
    private static final int INFINITE_FACTOR = 100;
    private static final int[][] UTILITY_TABLE = new int[2][Board.WINCOUNT + 1];// 4+1
    static {
        initUtilityTable();
    }
    /* evaluation system end ****** */
    private int currentPlayer;
    private int winner = Board.PLAYER_EMPTY;
    private Route[][][] winRoutesArray =
            new Route[MAX_ROUTE_TYPE_ID + 1][MAX_FIRST_INDEX + 1][MAX_SECOND_INDEX + 1];

    /* public method begin ******************************** */
    public static final int INFINITE_UTILITY = UTILITY_TABLE[TURN_DEFFENCE][Board.WINCOUNT];
    public WinRoutes() {
        currentPlayer = Board.PLAYER_X;
        for (int routeTypeID = 0; routeTypeID <= MAX_ROUTE_TYPE_ID; routeTypeID++) {
            for (int firstIndex = 0; firstIndex <= MAX_FIRST_INDEX; firstIndex++) {
                for (int secondIndex = 0; secondIndex <= MAX_SECOND_INDEX; secondIndex++) {
                    winRoutesArray[routeTypeID][firstIndex][secondIndex] =
                            new Route();
                }
            }
        }
    }

    public int getFirstRouteID () {
        return 0;
    }

    /**
     * @return: next route id. -1, if it's the last id.
     */
    public int getNextRouteID (int routeID) {
        int newRouteID = routeID;
        final int typeID = getRouteTypeID(routeID);
        switch (typeID) {
            case 0:
            case 1:
            case 2:
                newRouteID++;
                break;
            case 3:
            case 4:
            case 5:
                newRouteID++;
                if (getSecondIndex(newRouteID) > 2) {
                    newRouteID = clearSecondIndex(newRouteID);
                    newRouteID = addOneToFirstIndex(newRouteID); // carry bit
                }
                break;
            case 6:
                newRouteID = addOneToFirstIndex(newRouteID);
                if (getRouteTypeID(newRouteID) > MAX_ROUTE_TYPE_ID) {
                    newRouteID = -1; // it's the last id
                }
                break;
        }
        return newRouteID;
    }

    /**
     * When player made a move, the winRoutes should be update.
     * In update process, if there is a winner, the winner flag will be changed.
     */
    public void update (int levelID, int rowID, int columnID) {
        // update all straight routes of this spot
        updateStraightRoutes(levelID, rowID, columnID);
        // update all diagonal routes of this spot
        updateDiagonalRoutes(levelID, rowID, columnID);

        switchPlayer();
    }

    public int getUtility (int player) {
        final int turn;
        if (player == currentPlayer) {
            turn = TURN_ATTACK;
        } else {
            turn = TURN_DEFFENCE;
        }
        final int utilityOfPlayer = getUtilityOfOnePlayer(player, turn);
        final int utilityOfOpponent =
                getUtilityOfOnePlayer(Board.getOpponent(player),
                        getOtherTurn(turn));
        
        return utilityOfPlayer - utilityOfOpponent;
    }

    public int getWinner () {
        return winner;
    }

    public Object clone () {
        WinRoutes o = null;
        try {
            o = (WinRoutes) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        o.winRoutesArray =
                new Route[MAX_ROUTE_TYPE_ID + 1][MAX_FIRST_INDEX + 1][MAX_SECOND_INDEX + 1];
        for (int routeTypeID = 0; routeTypeID <= MAX_ROUTE_TYPE_ID; routeTypeID++) {
            for (int firstIndex = 0; firstIndex <= MAX_FIRST_INDEX; firstIndex++) {
                for (int secondIndex = 0; secondIndex <= MAX_SECOND_INDEX; secondIndex++) {
                    o.winRoutesArray[routeTypeID][firstIndex][secondIndex] =
                            (Route) winRoutesArray[routeTypeID][firstIndex][secondIndex]
                                    .clone();
                }
            }
        }
        return o;
    }

    /* public method end ******************************** */
    private static void initUtilityTable () {
        UTILITY_TABLE[TURN_DEFFENCE][0] = 0;
        UTILITY_TABLE[TURN_ATTACK][0] = 0;

        UTILITY_TABLE[TURN_DEFFENCE][1] = 1;

        UTILITY_TABLE[TURN_ATTACK][1] =
                UTILITY_TABLE[TURN_DEFFENCE][1] * OFFENSIVE_FACTOR;
        UTILITY_TABLE[TURN_DEFFENCE][2] =
                UTILITY_TABLE[TURN_ATTACK][1] * SUPERIORITY_FACTOR;
        UTILITY_TABLE[TURN_ATTACK][2] =
                UTILITY_TABLE[TURN_DEFFENCE][2] * OFFENSIVE_FACTOR;
        UTILITY_TABLE[TURN_DEFFENCE][3] =
                UTILITY_TABLE[TURN_ATTACK][2] * SUPERIORITY_FACTOR;
        UTILITY_TABLE[TURN_ATTACK][3] =
                UTILITY_TABLE[TURN_DEFFENCE][3] * OFFENSIVE_FACTOR;

        UTILITY_TABLE[TURN_DEFFENCE][4] =
                UTILITY_TABLE[TURN_ATTACK][3] * INFINITE_FACTOR;
        // Attack 4 won't happen
        UTILITY_TABLE[TURN_ATTACK][4] =
                UTILITY_TABLE[TURN_DEFFENCE][4];
    }

    private int getRouteTypeID (int routeID) {
        return routeID >> 4;
    }

    private int getFirstIndex (int routeID) {
        return (routeID >> 2) & 0x3;
    }

    private int getSecondIndex (int routeID) {
        return routeID & 0x3;
    }

    private int clearSecondIndex (int routeID) {
        return routeID & (~0x3);
    }

    private int addOneToFirstIndex (int routeID) {
        return routeID + (1 << 2);
    }

    private int getRouteID (int routeTypeID, int firstIndex, int secondIndex) {
        return (routeTypeID << 4) + (firstIndex << 2) + secondIndex;
    }

    private Route getRoute (int routeID) {
        final int routeTypeID = getRouteTypeID(routeID);
        final int firstIndex = getFirstIndex(routeID);
        final int secondIndex = getSecondIndex(routeID);
        return winRoutesArray[routeTypeID][firstIndex][secondIndex];
    }

    private void switchPlayer () {
        if (currentPlayer == Board.PLAYER_X) {
            currentPlayer = Board.PLAYER_O;
        } else {
            currentPlayer = Board.PLAYER_X;
        }
    }

    private void updateOneRoute (int routeTypeID, int firstIndex,
            int secondIndex) {
        final int routeID = getRouteID(routeTypeID, firstIndex, secondIndex);
        final Route route = getRoute(routeID);
        final boolean hasWinner = route.addOnePiece(currentPlayer);
        if (hasWinner) {
            winner = currentPlayer;
        }
    }

    private void updateStraightRoutes (int levelID, int rowID, int columnID) {
        // 0, level changes, other remain
        int routeTypeID = 0;
        int firstIndex = rowID;
        int secondIndex = columnID;
        updateOneRoute(routeTypeID, firstIndex, secondIndex);
        // 1, row changes, other remain
        routeTypeID = 1;
        firstIndex = levelID;
        secondIndex = columnID;
        updateOneRoute(routeTypeID, firstIndex, secondIndex);
        // 2, column changes, other remain
        routeTypeID = 2;
        firstIndex = levelID;
        secondIndex = rowID;
        updateOneRoute(routeTypeID, firstIndex, secondIndex);
    }

    private boolean isDirectRatio (int x, int y) {
        if (x == y) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isInverseRatio (int x, int y) {
        if (x + y == Board.MAX_ROW - 1) {// x+y==3
            return true;
        } else {
            return false;
        }
    }

    private boolean isAllDirectRatio (int x, int y, int z) {
        if (x == y && x == z) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isOneInverseRatio (int x, int y, int z) {
        // x is inverse, y and z are direct
        if (y == z && (x + y == Board.MAX_ROW - 1)) {
            return true;
        } else {
            return false;
        }
    }

    private void updateDiagonalRoutes (int levelID, int rowID, int columnID) {
        // 3, level remains, others change
        int routeTypeID = 3;
        int firstIndex = levelID;
        int secondIndex;
        if (isDirectRatio(rowID, columnID)) {
            secondIndex = ROUTE_TYPE_DIRECT_RATIO;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        if (isInverseRatio(rowID, columnID)) {
            secondIndex = ROUTE_TYPE_INVERSE_RATIO;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        // 4, row remains, others change
        routeTypeID = 4;
        firstIndex = rowID;
        if (isDirectRatio(levelID, columnID)) {
            secondIndex = ROUTE_TYPE_DIRECT_RATIO;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        if (isInverseRatio(levelID, columnID)) {
            secondIndex = ROUTE_TYPE_INVERSE_RATIO;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        // 5, column remains, others change
        routeTypeID = 5;
        firstIndex = columnID;
        if (isDirectRatio(levelID, rowID)) {
            secondIndex = ROUTE_TYPE_DIRECT_RATIO;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        if (isInverseRatio(levelID, rowID)) {
            secondIndex = ROUTE_TYPE_INVERSE_RATIO;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }

        // 6, all change
        routeTypeID = 6;
        secondIndex = 0; // this case do not use secondIndex
        if (isAllDirectRatio(levelID, rowID, columnID)) {
            firstIndex = 0;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        if (isOneInverseRatio(levelID, rowID, columnID)) {
            firstIndex = 1;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        if (isOneInverseRatio(rowID, levelID, columnID)) {
            firstIndex = 2;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
        if (isOneInverseRatio(columnID, levelID, rowID)) {
            firstIndex = 3;
            updateOneRoute(routeTypeID, firstIndex, secondIndex);
        }
    }

    private static int getOtherTurn (int turn) {
        if (turn == TURN_ATTACK) {
            return TURN_DEFFENCE;
        } else {
            return TURN_ATTACK;
        }
    }

    private int getUtilityOfOnePlayer (int player, int turn) {
        int utility = 0;
        // scan all routes, get utility sum of this player
        int routeID = getFirstRouteID();
        while (routeID != -1) {
            final Route route = getRoute(routeID);
            final int opponent = Board.getOpponent(player);
            /* if there is piece from opponent in this route, this route
             * will never become a win route, so it's no utility. */
            if (route.getNumOfPieces(opponent) == 0) {
                final int pieces = route.getNumOfPieces(player);
                // add the utility of this route into sum
                utility += UTILITY_TABLE[turn][pieces];
                assert !(turn == TURN_ATTACK && pieces == Board.WINCOUNT);
            }

            routeID = getNextRouteID(routeID);
        }
        return utility;
    }
}
