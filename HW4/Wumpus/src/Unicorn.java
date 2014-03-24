public class Unicorn {
    private static final String R1 = "Mythical => Immortal";
    private static final String R2 = "!Mythical => Mammal";
    private static final String R3 = "(Mythical | Mammal) => Horned";
    private static final String R4 = "Horned => Magical";
    private static final String Q1 = "Mythical";
    private static final String Q2 = "Magical";
    private static final String Q3 = "Horned";

    public static void main (final String[] args) {
        final KnowledgeBase kb = new KnowledgeBase();
        kb.tellKB(R1);
        kb.tellKB(R2);
        kb.tellKB(R3);
        kb.tellKB(R4);
        System.out.println(kb.askKB(Q1));
        System.out.println(kb.askKB(Q2));
        System.out.println(kb.askKB(Q3));
    }
}
