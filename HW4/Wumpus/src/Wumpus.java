public class Wumpus {
    private static final String R1 = "!P11";
    private static final String R2 = "B11 <=> (P12 | P21)";
    private static final String R3 = "B21 <=> (P11 | P22 | P31)";
    private static final String R4 = "!B11";
    private static final String R5 = "B21";
    private static final String Q1 = "!P12";
    private static final String Q2 = "!P22";

    public static void main (final String[] args) {
        final KnowledgeBase kb = new KnowledgeBase();
        kb.tellKB(R1);
        kb.tellKB(R2);
        kb.tellKB(R3);
        kb.tellKB(R4);
        kb.tellKB(R5);
        System.out.println(kb.askKB(Q1));
        System.out.println(kb.askKB(Q2));
    }
}
