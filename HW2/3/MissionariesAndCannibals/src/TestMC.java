import java.util.ArrayList;

public class TestMC {
    private static final int MAX_NUM_OF_M = 3;
    private static final int MAX_NUM_OF_C = 3;
    private static final int LEFT_BANK = 0;
    private static final int RIGHT_BANK = 1;
    private static final String LEFT_BANK_STRING = "left";
    private static final String RIGHT_BANK_STRING = "right";
    private static final int COST_PER_M = 1;
    private static final int COST_PER_C = 2;
    private static final int INVALID_VALUE = -1;

    /**** state begin ********************************************/
    private static class State {
        int m;
        int c;
        int bank;
        int parentM;
        int parentC;
        int parentBank;
        int costG;
    }

    private static String getBankString (final int bank) {
        final String result;
        if (bank == LEFT_BANK) {
            result = LEFT_BANK_STRING;
        } else {
            result = RIGHT_BANK_STRING;
        }
        return result;
    }

    private static int getBankNum (final String bank) {
        final int result;
        if (bank.equalsIgnoreCase(LEFT_BANK_STRING)) {
            result = LEFT_BANK;
        } else {
            result = RIGHT_BANK;
        }
        return result;
    }

    private static State initState (final int m, final int c, final int bank) {
        State state = new State();
        state.m = m;
        state.c = c;
        state.bank = bank;
        state.parentM = INVALID_VALUE;
        state.parentC = INVALID_VALUE;
        state.parentBank = INVALID_VALUE;
        state.costG = 0;
        return state;
    }

    private static boolean goalTest (final State state) {
        if ((state.m == MAX_NUM_OF_M) && (state.c == MAX_NUM_OF_C)
                && (state.bank == RIGHT_BANK)) {
            return true;
        } else {
            return false;
        }
    }

    private static boolean isSameStates (final State state1, final State state2) {
        if ((state1.m == state2.m) && (state1.c == state2.c)
                && (state1.bank == state2.bank)) {
            return true;
        } else {
            return false;
        }
    }

    private static final State generateChild (final State parent) {
        final State child = new State();
        child.m = MAX_NUM_OF_M - parent.m;
        child.c = MAX_NUM_OF_C - parent.c;
        // move to the other bank
        if (parent.bank == LEFT_BANK) {
            child.bank = RIGHT_BANK;
        } else {
            child.bank = LEFT_BANK;
        }
        child.costG = parent.costG;
        child.parentM = parent.m;
        child.parentC = parent.c;
        child.parentBank = parent.bank;
        return child;
    }

    /**** state end ********************************************/

    /**** actions begin ********************************************/
    private static State boat1M (final State node) {
        final State newNode = generateChild(node);
        newNode.m += 1;
        newNode.costG += COST_PER_M;
        return newNode;
    }

    private static State boat2M (final State node) {
        final State newNode = generateChild(node);
        newNode.m += 2;
        newNode.costG += 2 * COST_PER_M;
        return newNode;
    }

    private static State boat1C (final State node) {
        final State newNode = generateChild(node);
        newNode.c += 1;
        newNode.costG += COST_PER_C;
        return newNode;
    }

    private static State boat2C (final State node) {
        final State newNode = generateChild(node);
        newNode.c += 2;
        newNode.costG += 2 * COST_PER_C;
        return newNode;
    }

    private static State boat1M1C (final State node) {
        final State newNode = generateChild(node);
        newNode.m += 1;
        newNode.c += 1;
        newNode.costG += COST_PER_M + COST_PER_C;
        return newNode;
    }

    /**** actions end ********************************************/

    /**** A* search begin ****************************************/
    private static int leftBankHeuristic (final State state) {
        final int costH;
        final int remainingM;
        final int remainingC;
        final int CostOfboatman;
        if (state.m != 0) {
            // boatman is missionary
            remainingM = state.m - 1;
            remainingC = state.c;
            CostOfboatman = COST_PER_M;
        } else {
            // boatman is cannibal
            remainingM = state.m;
            remainingC = state.c - 1;
            CostOfboatman = COST_PER_C;
        }
        costH = ((remainingM + remainingC) * 2 - 1) * CostOfboatman
                + (remainingM * COST_PER_M) + (remainingC * COST_PER_C);

        return costH;
    }

    private static State rightToLeft (final State state) {
        final State leftBankState = generateChild(state);
        if (state.m != 0) {
            // send a missionary to left
            leftBankState.m += 1;
        } else {
            // if no missionary can be send, send a cannibal to left
            leftBankState.c += 1;
        }
        return leftBankState;
    }

    private static int costOfRightToLeft (final State state) {
        final int cost;
        if (state.m != 0) {
            // send a missionary to left
            cost = COST_PER_M;
        } else {
            // if no missionary can be send, send a cannibal to left
            cost = COST_PER_C;
        }
        return cost;
    }

    private static int heuristic (final State state, final boolean isAStar) {
        if (!isAStar) {
            return 0;
        }
        int costH = 0;
        final State leftBankState;
        if (state.bank == RIGHT_BANK) {
            // return to left bank state
            leftBankState = rightToLeft(state);
            costH += costOfRightToLeft(state);
        } else {
            leftBankState = state;
        }
        costH += leftBankHeuristic(leftBankState);
        return costH;
    }

    private static ArrayList<State> filterInvalidState (
            final ArrayList<State> states) {
        final ArrayList<State> result = new ArrayList<State>();
        for (int i = 0; i < states.size(); i++) {
            final State node = states.get(i);
            boolean isValid = true;
            if ((node.c > MAX_NUM_OF_C) || (node.m > MAX_NUM_OF_M)) {
                isValid = false;
            }
            // current bank
            if (((node.m != 0) && (node.c > node.m))) {
                isValid = false;
            }
            // other side of the bank
            if (((MAX_NUM_OF_M - node.m) != 0)
                    && ((MAX_NUM_OF_M - node.c) > (MAX_NUM_OF_M - node.m))) {
                isValid = false;
            }

            if (isValid) {
                result.add(node);
            }
        }
        return result;
    }

    private static ArrayList<State> expandNodes (final State node) {
        final ArrayList<State> result = new ArrayList<State>();

        result.add(boat1M(node));
        result.add(boat2M(node));
        result.add(boat1C(node));
        result.add(boat2C(node));
        result.add(boat1M1C(node));
        // only return valid states
        return filterInvalidState(result);
    }

    private static ArrayList<State> filterNodes (
            final ArrayList<State> expanded, final ArrayList<State> explored) {
        final ArrayList<State> result = new ArrayList<State>();
        for (int i = 0; i < expanded.size(); i++) {
            // to check if the node has existed in the explored queue
            boolean hasExisted = false;
            for (int j = 0; j < explored.size(); j++) {
                if (isSameStates(expanded.get(i), explored.get(j))) {
                    hasExisted = true;
                    break;
                }
            }
            // if the node hasn't been explored, keep it; otherwise, discard it.
            if (!hasExisted) {
                result.add(expanded.get(i));
            }
        }
        return result;
    }

    private static void showSearchPath (final ArrayList<State> queue,
            final boolean isAStar) {
        while (!queue.isEmpty()) {
            final State node = queue.remove(0);
            System.out.printf(
                    "[%d,%d,%5s] costG: %2d costF: %2d parent: [%d,%d]%n",
                    node.m, node.c, getBankString(node.bank), node.costG,
                    getCost(node, isAStar), node.parentM, node.parentC);
        }
    }

    private static void showResult (final ArrayList<State> explored,
            final ArrayList<State> frontier, final boolean hasSolved,
            final boolean isAStar) {
        if (hasSolved) {
            ArrayList<State> solutionPath = getSolutionPath(explored);
            System.out.printf(
                    "solution costs %d. number of step is: %d. the path is:%n",
                    solutionPath.get(solutionPath.size() - 1).costG,
                    solutionPath.size() - 1);
            for (int i = 0; i < solutionPath.size(); i++) {
                System.out.printf("[%d,%d,%s] ", solutionPath.get(i).m,
                        solutionPath.get(i).c,
                        getBankString(solutionPath.get(i).bank));
            }
            System.out.printf("%n");
        }

        System.out.printf("explored nodes: %d%n", explored.size());
        showSearchPath(explored, isAStar);
        System.out.printf("unexplored but generated nodes: %d%n",
                frontier.size());
        showSearchPath(frontier, isAStar);
    }

    private static ArrayList<State> getSolutionPath (
            final ArrayList<State> explored) {
        ArrayList<State> solutionPath = new ArrayList<State>();
        final int size = explored.size();
        assert (size > 0);
        // get the last one, the goal state
        State node = explored.get(size - 1);
        solutionPath.add(0, node);
        while (node.parentM != -1) {
            // find the parent of current node
            for (int i = 0; i < size; i++) {
                if ((explored.get(i).m == node.parentM)
                        && (explored.get(i).c == node.parentC)
                        && (explored.get(i).bank == node.parentBank)) {
                    node = explored.get(i);
                    break;
                }
            }
            solutionPath.add(0, node);
        }
        return solutionPath;
    }

    private static int getCost (final State state, final boolean isAStar) {
        return state.costG + heuristic(state, isAStar);
    }

    private static int getIndexHasMinCostF (final ArrayList<State> queue,
            final boolean isAStar) {
        int index = -1;
        int minCost = Integer.MAX_VALUE;
        for (int i = 0; i < queue.size(); i++) {
            final int cost = getCost(queue.get(i), isAStar);
            if (minCost > cost) {
                minCost = cost;
                index = i;
            }
        }
        assert (index != -1);
        return index;
    }

    private static int indexOf (final State node, final ArrayList<State> queue) {
        int result = -1;
        for (int i = 0; i < queue.size(); i++) {
            if (isSameStates(node, queue.get(i))) {
                result = i;
                break;
            }
        }
        return result;
    }

    private static void replaceNodeWithLowerCost (final State node,
            final int index, final ArrayList<State> queue) {
        if (node.costG < queue.get(index).costG) {
            queue.set(index, node);
        }
    }

    private static void addResultstoFrontier (ArrayList<State> result,
            ArrayList<State> frontier) {
        for (int i = 0; i < result.size(); i++) {
            final State tempNode = result.get(i);
            final int index = indexOf(tempNode, frontier);
            if (index != -1) {
                /*
                 * if node exists in the frontier and has a lower costG,
                 * replace the frontier one with node
                 */
                replaceNodeWithLowerCost(tempNode, index, frontier);
            } else {
                // if node doesn't exist in the frontier, add it
                frontier.add(tempNode);
            }
        }
    }

    private static void generalSearch (State initState, boolean isAStar) {
        final ArrayList<State> frontier = new ArrayList<State>();
        final ArrayList<State> explored = new ArrayList<State>();
        boolean hasSolved = false;

        // initialize the frontier using the initial state of problem
        frontier.add(initState);

        // if the frontier is empty then return failure
        while (!frontier.isEmpty()) {
            /*
             * choose a leaf node with lowest cost and remove it from the
             * frontier
             */
            final State node = frontier.remove(getIndexHasMinCostF(frontier,
                    isAStar));
            // the node is about to be explored
            explored.add(node);
            /*
             * if the node contains a goal state then return the corresponding
             * solution
             */
            if (goalTest(node)) {
                hasSolved = true;
                break;
            }
            // else expand the chosen node
            ArrayList<State> expanded = expandNodes(node);
            // delete the nodes have already been explored
            expanded = filterNodes(expanded, explored);
            // add the resulting nodes to the frontier
            addResultstoFrontier(expanded, frontier);
        }

        showResult(explored, frontier, hasSolved, isAStar);
    }

    /**** A* search end ********************************************/
    private static final String METHOD_ASTAR = "aStar";

    // private static final String METHOD_UCS = "UCS";

    /*
     * sample commad line:
     * java TestMC 3 3 left astar
     * java TestMC 1 1 right ucs
     */
    public static void main (String[] args) {
        final int m = Integer.parseInt(args[0]);
        final int c = Integer.parseInt(args[1]);
        final int bank = getBankNum(args[2]);
        final String method = args[3];
        final boolean isAStar;

        State initialState = initState(m, c, bank);
        if (method.equalsIgnoreCase(METHOD_ASTAR)) {
            isAStar = true;
        } else { // if (method.equalsIgnoreCase(METHOD_UCS)) {
            isAStar = false;
        }

        generalSearch(initialState, isAStar);
    }
}
