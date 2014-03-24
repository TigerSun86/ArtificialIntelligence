import java.util.ArrayList;

public class TestJug {
    private static final int MAX_WATER_IN_SMALL = 3;
    private static final int MAX_WATER_IN_LARGE = 5;
    private static final int GOAL_WATER_IN_LARGE = 4;

    /**** state begin ********************************************/
    private static class State {
        int s;
        int l;
        int costG;
        int parentS;
        int parentL;
    }

    private static State initState (final int s, final int l) {
        State state = new State();
        state.s = s;
        state.l = l;
        state.costG = 0;
        state.parentS = -1;
        state.parentL = -1;
        return state;
    }

    private static boolean goalTest (final State state) {
        if (state.l == GOAL_WATER_IN_LARGE) {
            return true;
        } else {
            return false;
        }
    }

    private static boolean isSameStates (final State state1, final State state2) {
        if ((state1.s == state2.s) && (state1.l == state2.l)) {
            return true;
        } else {
            return false;
        }
    }

    private static final State generateChild (final State parent) {
        final State child = new State();
        child.s = parent.s;
        child.l = parent.l;
        child.costG = parent.costG;
        child.parentS = parent.s;
        child.parentL = parent.l;
        return child;
    }

    /**** state end ********************************************/

    /**** actions begin ********************************************/
    private static State fillSmall (final State node) {
        final State newNode = generateChild(node);
        newNode.s = MAX_WATER_IN_SMALL;
        newNode.costG += 1 + (MAX_WATER_IN_SMALL - node.s);
        return newNode;
    }

    private static State fillLarge (final State node) {
        final State newNode = generateChild(node);
        newNode.l = MAX_WATER_IN_LARGE;
        newNode.costG += 1 + (MAX_WATER_IN_LARGE - node.l);
        return newNode;
    }

    private static State emptySmall (final State node) {
        final State newNode = generateChild(node);
        newNode.s = 0;
        newNode.costG += 1 + (node.s * 2);
        return newNode;
    }

    private static State emptyLarge (final State node) {
        final State newNode = generateChild(node);
        newNode.l = 0;
        newNode.costG += 1 + (node.l * 2);
        return newNode;
    }

    private static State pourSmalltoLarge (final State node) {
        final State newNode = generateChild(node);
        if ((node.s + node.l) > MAX_WATER_IN_LARGE) {
            // small has remainder and large is full
            newNode.s = node.s + node.l - MAX_WATER_IN_LARGE;
            newNode.l = MAX_WATER_IN_LARGE;
        } else {
            // small is empty and large has total amount of water
            newNode.s = 0;
            newNode.l = node.s + node.l;
        }
        newNode.costG += 1 + (newNode.l - node.l);
        return newNode;
    }

    private static State pourLargetoSmall (final State node) {
        final State newNode = generateChild(node);
        if ((node.s + node.l) > MAX_WATER_IN_SMALL) {
            // small is full and large has remainder
            newNode.s = MAX_WATER_IN_SMALL;
            newNode.l = node.s + node.l - MAX_WATER_IN_SMALL;
        } else {
            // small has total amount of water and large is empty
            newNode.s = node.s + node.l;
            newNode.l = 0;
        }
        newNode.costG += 1 + (newNode.s - node.s);
        return newNode;
    }

    /**** actions end ********************************************/

    /**** A* search begin ****************************************/
    private static int heuristic (final State state, final boolean isAStar) {
        if (!isAStar) {
            return 0;
        }
        final int costH;
        if (state.l < GOAL_WATER_IN_LARGE) {
            costH = 1 + GOAL_WATER_IN_LARGE - state.l;
        } else if (state.l > GOAL_WATER_IN_LARGE) {
            costH = 1 + state.l - GOAL_WATER_IN_LARGE;
        } else {
            costH = 0;
        }
        return costH;
    }

    private static ArrayList<State> expandNodes (final State node) {
        final ArrayList<State> result = new ArrayList<State>();

        result.add(fillSmall(node));
        result.add(fillLarge(node));
        result.add(emptySmall(node));
        result.add(emptyLarge(node));
        result.add(pourSmalltoLarge(node));
        result.add(pourLargetoSmall(node));

        return result;
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

    private static void showSearchPath (ArrayList<State> queue, boolean isAStar) {
        while (!queue.isEmpty()) {
            final State node = queue.remove(0);
            System.out.printf(
                    "[%d,%d] costG: %2d costF: %2d parent: [%d,%d]%n", node.s,
                    node.l, node.costG, getCostGreedy(node, isAStar), node.parentS,
                    node.parentL);
        }
    }

    private static void showResult (ArrayList<State> explored,
            ArrayList<State> frontier, boolean hasSolved, boolean isAStar) {
        if (hasSolved) {
            ArrayList<State> solutionPath = getSolutionPath(explored);
            System.out.printf("solution costs %d. the path is:%n",
                    solutionPath.get(solutionPath.size() - 1).costG);
            for (int i = 0; i < solutionPath.size(); i++) {
                System.out.printf("[%d,%d] ", solutionPath.get(i).s,
                        solutionPath.get(i).l);
            }
            System.out.printf("%n");
        }

        System.out.printf("explored nodes: %d%n", explored.size());
        showSearchPath(explored, isAStar);
        System.out.printf("unexplored but generated nodes: %d%n",
                frontier.size());
        showSearchPath(frontier, isAStar);
    }

    private static ArrayList<State> getSolutionPath (ArrayList<State> explored) {
        ArrayList<State> solutionPath = new ArrayList<State>();
        final int size = explored.size();
        assert (size > 0);
        // get the last one, the goal state
        State node = explored.get(size - 1);
        solutionPath.add(0, node);
        while (node.parentS != -1) {
            // find the parent of current node
            for (int i = 0; i < size; i++) {
                if ((explored.get(i).s == node.parentS)
                        && (explored.get(i).l == node.parentL)) {
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
    private static int getCostGreedy (final State state, final boolean isAStar) {
        return heuristic(state, isAStar);
    }

    private static int getIndexHasMinCostF (final ArrayList<State> queue,
            final boolean isAStar) {
        int index = -1;
        int minCost = Integer.MAX_VALUE;
        for (int i = 0; i < queue.size(); i++) {
            final int cost = getCostGreedy(queue.get(i), isAStar);
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
             * choose a leaf node with lowest costG and remove it from the
             * frontier
             */
            final State node = frontier.remove(getIndexHasMinCostF(frontier,
                    isAStar));
            // the node is about to be explored
            explored.add(node);
            // if the node contains a goal state then return the corresponding
            // solution
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
     * java TestJug 0 0 astar
     * java TestJug 0 5 usc
     */

    public static void main (String[] args) {
        final int s = Integer.parseInt(args[0]);
        final int l = Integer.parseInt(args[1]);
        final String method = args[2];
        final boolean isAStar;

        State initialState = initState(s, l);
        if (method.equalsIgnoreCase(METHOD_ASTAR)) {
            isAStar = true;
        } else { // if (method.equalsIgnoreCase(METHOD_UCS)) {
            isAStar = false;
        }
        generalSearch(initialState, isAStar);
    }
}
