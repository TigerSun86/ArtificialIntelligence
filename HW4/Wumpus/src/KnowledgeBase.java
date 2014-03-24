import java.util.ArrayList;

public class KnowledgeBase {
    private ArrayList<Clause> kb;

    public KnowledgeBase() {
        kb = new ArrayList<Clause>();
    }

    public final void tellKB (final String percepts) {
        addPercepts(kb, percepts);
    }

    public final boolean askKB (final String query) {
        final ArrayList<Clause> clauseSet = new ArrayList<Clause>();
        /* I need a new copy of knowledge base for resolution, because I don't
         * want to change original kb. */
        clauseSet.addAll(kb);
        final String negQuery = getNegQuery(query);
        addPercepts(clauseSet, negQuery);
        final ArrayList<Clause> newClauses = new ArrayList<Clause>();

        while (true) {
            for (int i = 0; i < clauseSet.size(); i++) {
                for (int j = i + 1; j < clauseSet.size(); j++) {
                    // Deal with each pair of clauses
                    final Clause resolvents =
                            Clause.resolve(clauseSet.get(i), clauseSet.get(j));
                    if (resolvents != null) {
                        // There is a new resolved clause
                        if (resolvents.isEmpty()) {
                            // Proved the query
                            return true;
                        }
                        if (!newClauses.contains(resolvents)) {
                            newClauses.add(resolvents);
                        }
                    }
                }
            }
            if (Clause.isSubSet(newClauses, clauseSet)) {
                // Have no enough knowledge to prove query
                return false;
            }
            // Add new clauses, and next loop scan all clauses
            Clause.add(clauseSet, newClauses);
        }
    }

    private void addPercepts (final ArrayList<Clause> clauseSet,
            final String percepts) {
        final ArrayList<Clause> newClauses = new ArrayList<Clause>();
        // Convert percepts into CNF clauses
        final String cnfSentence = CNFconvert.getCNF(percepts);
        // We need a extractor to get clauses from a string
        final CNFExtractor cnfClauses = new CNFExtractor(cnfSentence);
        while (cnfClauses.hasNext()) {
            // Extract the clause
            newClauses.add(new Clause(cnfClauses.getNext()));
        }
        // Add new clauses into clause set
        Clause.add(clauseSet, newClauses);
    }

    private String getNegQuery (final String query) {
        final String convertedQ;
        if (query.charAt(0) == '!') {
            // If it has a '!' in front, just delete it
            convertedQ = query.replace("!", "");
        } else {
            // Add one '!' in front
            convertedQ = "!" + "(" + query + ")";
        }
        // Convert it into CNF clauses
        return CNFconvert.getCNF(convertedQ);
    }
}
