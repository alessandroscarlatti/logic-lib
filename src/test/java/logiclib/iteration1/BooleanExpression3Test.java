package logiclib.iteration1;

import org.junit.Test;

import static logiclib.iteration1.BooleanExpression3.bln;
import static logiclib.iteration1.BooleanExpression3.not;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class BooleanExpression3Test {

    @Test
    public void testSyntax() {

//        class Facts {
//            boolean a;
//            boolean b;
//            boolean c;
//            boolean d;
//        }
//
//        BooleanExpression33 philLikesRed = bln(f -> ((Facts) f).a.equals("RED"), "Phil likes red");
//        BooleanExpression33 annieLikesRed = bln(f -> ((Facts) f).b.equals("RED"), "Annie likes red");
//        BooleanExpression33 charlotteLikesBlue = bln(f -> ((Facts) f).c.equals("BLUE"), "Charlotte likes blue");
//        BooleanExpression33 teddyLikesBlue = bln(f -> ((Facts) f).d.equals("BLUE"), "Teddy likes blue");
//
//        BooleanExpression33 expr = bln(
//                philLikesRed.and(annieLikesRed)
//        ).or(
//                charlotteLikesBlue.and(teddyLikesBlue)
//        );
//
//        Facts facts = new Facts();
//        facts.a = true;
//        facts.b = true;
//        facts.c = true;
//        facts.d = true;
//
//        boolean result = expr.evaluate(facts);
    }

    @Test
    public void testToString() {
        BooleanExpression3 a = bln(true, "A");
        System.out.println("A,true: " + a);

        BooleanExpression3 aAndB = bln(true, "A")
                .and(bln(true, "B"));
        System.out.println("A,true AND B,true: " + aAndB);

        BooleanExpression3 aOrB = bln(true, "A")
                .or(bln(false, "B"));
        System.out.println("A,true OR B,true: " + aOrB);

        BooleanExpression3 aAndBorC = bln(true, "A")
                .and(bln(true, "B"))
                .or(bln(true, "C"));
        System.out.println("A,true AND B,true OR C,true: " + aAndBorC);

        BooleanExpression3 aAndB_or_cAndD = bln(
                bln(true, "A").and(
                        bln(true, "B"))
        ).or(bln(true, "C").and(
                bln(true, "D")));

        System.out.println("(A,true AND B,true) OR (C,true and D,true): " + aAndB_or_cAndD);

        BooleanExpression3 aAndBorCAndD =
                bln((f) -> 5 < 10, "A").and(
                        bln(true, "B")).or(
                        bln(true, "C")).and(
                        bln(true, "D"));

        System.out.println("(A,true AND B,true OR C,true and D,true): " + aAndBorCAndD);
    }

    @Test
    public void testLogic() {
        assertTrue(bln(true, "A")
                .and(not(false, "B"))
                .and(not(false, "C"))
                .evaluate(null));

        assertFalse(bln(true, "A")
                .and(not(false, "B"))
                .and(not(true, "C"))
                .evaluate(null));

        assertFalse(bln(false, "A")
                .and(not(false, "B"))
                .and(not(true, "C"))
                .evaluate(null));

        assertFalse(bln(false, "A")
                .and(not(true, "B"))
                .and(not(true, "C"))
                .evaluate(null));

        assertTrue(bln((Integer f) -> f > 10, "input > 10")
                .evaluate(11));

        class PenguinFacts {
            String name;
            int age;
            String favoriteColor;

            public PenguinFacts(String name, int age, String favoriteColor) {
                this.name = name;
                this.age = age;
                this.favoriteColor = favoriteColor;
            }
        }

        BooleanExpression3 isTheRealPhil = bln((PenguinFacts f) -> f.name.equals("Phil"), "Name is Phil")
                .and((PenguinFacts f) -> f.age == 2, "Age is 2, because the real Phil is 2")
                .and(not((PenguinFacts f) -> f.favoriteColor.equals("Red"), "Favorite color is red"));

        assertFalse(isTheRealPhil.evaluate(new PenguinFacts("Annie", 2, "Red")));
        assertFalse(isTheRealPhil.evaluate(new PenguinFacts("Phil", 3, "Red")));
        assertTrue(isTheRealPhil.evaluate(new PenguinFacts("Phil", 2, "Blue")));

        System.out.println("done");
    }
}
