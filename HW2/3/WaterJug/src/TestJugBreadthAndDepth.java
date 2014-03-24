import java.util.ArrayDeque;

public class TestJugBreadthAndDepth {
    private static final int MAX_WATER_IN_SMALL = 3;
    private static final int MAX_WATER_IN_LARGE = 5;
    private static final int GOAL_WATER_IN_LARGE = 4;
    
    /**** state begin ********************************************/
    private static class State {
        int s;
        int l;
    }
    
    private static boolean goalTest (State state) {
        if (state.l == GOAL_WATER_IN_LARGE) {
            return true;
        } else {
            return false;
        }
    }

    private static boolean isSameStates (State state1, State state2) {
        if ((state1.s == state2.s) && (state1.l == state2.l)) {
            return true;
        } else {
            return false;
        }
    }

    private static void stateCopy (State dec, State src) {
        dec.s = src.s;
        dec.l = src.l;
    }
    /**** state end ********************************************/
    
    /**** actions begin ********************************************/
    private static State fillSmall (State node) {
        State newNode = new State();
        stateCopy(newNode, node);
        newNode.s = MAX_WATER_IN_SMALL;
        return newNode;
    }

    private static State fillLarge (State node) {
        State newNode = new State();
        stateCopy(newNode, node);
        newNode.l = MAX_WATER_IN_LARGE;
        return newNode;
    }

    private static State emptySmall (State node) {
        State newNode = new State();
        stateCopy(newNode, node);
        newNode.s = 0;
        return newNode;
    }

    private static State emptyLarge (State node) {
        State newNode = new State();
        stateCopy(newNode, node);
        newNode.l = 0;
        return newNode;
    }

    private static State pourSmalltoLarge (State node) {
        State newNode = new State();

        if ((node.s + node.l) > MAX_WATER_IN_LARGE) {
            // small has remainder and large is full
            newNode.s = node.s + node.l - MAX_WATER_IN_LARGE;
            newNode.l = MAX_WATER_IN_LARGE;
        } else {
            // small is empty and large has total amount of water
            newNode.s = 0;
            newNode.l = node.s + node.l;
        }

        return newNode;
    }

    private static State pourLargetoSmall (State node) {
        State newNode = new State();

        if ((node.s + node.l) > MAX_WATER_IN_SMALL) {
            // small is full and large has remainder
            newNode.s = MAX_WATER_IN_SMALL;
            newNode.l = node.s + node.l - MAX_WATER_IN_SMALL;
        } else {
            // small has total amount of water and large is empty
            newNode.s = node.s + node.l;
            newNode.l = 0;
        }

        return newNode;
    }
    /**** actions end ********************************************/

    /**** breadth first or depth first search begin ************/
    private static ArrayDeque<State> expandNodes (State node, boolean needQueue) {
        final ArrayDeque<State> result = new ArrayDeque<State>();
        if (needQueue == true) {
            result.add(fillSmall(node));
            result.add(fillLarge(node));
            result.add(emptySmall(node));
            result.add(emptyLarge(node));
            result.add(pourSmalltoLarge(node));
            result.add(pourLargetoSmall(node));
        } else { // needStack
            result.push(fillSmall(node));
            result.push(fillLarge(node));
            result.push(emptySmall(node));
            result.push(emptyLarge(node));
            result.push(pourSmalltoLarge(node));
            result.push(pourLargetoSmall(node));
        }

        return result;
    }
    
    private static void filterNodes (ArrayDeque<State> result,
            ArrayDeque<State> explored) {
        final int resultsNumber = result.size(); // size may change dynamically
        for (int i = 0; i < resultsNumber; i++) {
            final State node = result.remove();
            boolean isExplored = false;
            // to check if the node has existed in the explored queue
            final int exploredNumber = explored.size();
            for (int j = 0; j < exploredNumber; j++) {
                final State exploredNode = explored.remove();
                // I just want to have a look, so don't forget to add it back
                explored.add(exploredNode);
                if (isSameStates(node, exploredNode)) {
                    isExplored = true;
                    /* here I didn't break because I don't want to change the
                       node order in explored queue */
                }
            }
            // if the node hasn't been explored, keep it; otherwise, discard it.
            if (!isExplored) {
                result.add(node);
            }
        }
    }
    
    private static void showSearchPath (ArrayDeque<State> explored) {
        while (!explored.isEmpty()) {
            final State node = explored.remove();
            System.out.printf("[%d,%d]%n", node.s, node.l);
        }
    }
    
    private static void bForDFSearch (State initState, boolean useQueue) {
        final ArrayDeque<State> frontier = new ArrayDeque<State>();
        final ArrayDeque<State> explored = new ArrayDeque<State>();
        // initialize the frontier using the initial state of problem
        if (useQueue) {
            frontier.add(initState);
        } else { // use stack
            frontier.push(initState);
        }

        // if the frontier is empty then return failure
        while (!frontier.isEmpty()) {
            // choose a leaf node and remove it from the frontier
            final State node;
            if (useQueue) {
                node = frontier.remove();
            } else { // use stack
                node = frontier.pop();
            }
            // the node is about to be explored
            explored.add(node);
            // if the node contains a goal state then return the corresponding
            // solution
            if (goalTest(node)) {
                break;
            }
            // else expand the chosen node
            final ArrayDeque<State> result = expandNodes(node, useQueue);
            // delete the nodes have already been explored
            filterNodes(result, explored);
            // also I don't need to add a node into frontier twice
            filterNodes(result, frontier);
            // add the resulting nodes to the frontier
            while (!result.isEmpty()) {
                if (useQueue) {
                    frontier.add(result.remove());
                } else { // use stack
                    frontier.push(result.pop());
                }
            }
        }

        showSearchPath(explored);
    }
    /**** breadth first or depth first search end ************/
    private static void breadthFirstSearch (State initState) {
        bForDFSearch(initState, true);
    }
    private static void depthFirstSearch (State initState) {
        bForDFSearch(initState, false);
    }

    public static void main (String[] args) {
        State initState = new State();
        initState.s = 0;
        initState.l = 0;
        breadthFirstSearch(initState);
        depthFirstSearch(initState);
    }
}
