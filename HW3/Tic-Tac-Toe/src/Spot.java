public class Spot implements Cloneable {
    int level;
    int row;
    int column;
    
    public Object clone () {
        Spot o = null;
        try {
            o = (Spot) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        return o;
    }
}