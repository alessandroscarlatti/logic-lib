package logiclib.iteration1.expression;

import org.junit.Test;

import static logiclib.iteration1.expression.BooleanExpression5.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class BooleanExpression5Test {

    @Test
    public void testSyntax() {

//        class Facts {
//            boolean a;
//            boolean b;
//            boolean c;
//            boolean d;
//        }
//
//        BooleanExpression53 philLikesRed = bln(f -> ((Facts) f).a.equals("RED"), "Phil likes red");
//        BooleanExpression53 annieLikesRed = bln(f -> ((Facts) f).b.equals("RED"), "Annie likes red");
//        BooleanExpression53 charlotteLikesBlue = bln(f -> ((Facts) f).c.equals("BLUE"), "Charlotte likes blue");
//        BooleanExpression53 teddyLikesBlue = bln(f -> ((Facts) f).d.equals("BLUE"), "Teddy likes blue");
//
//        BooleanExpression53 expr = bln(
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
        BooleanExpression5 a = bln(true, "A");
        System.out.println("A,true: " + a);

        BooleanExpression5 aAndB = bln(true, "A")
                .and(bln(true, "B"));
        System.out.println("A,true AND B,true: " + aAndB);

        BooleanExpression5 aOrB = bln(true, "A")
                .or(bln(false, "B"));
        System.out.println("A,true OR B,true: " + aOrB);

        BooleanExpression5 aAndBorC = bln(true, "A")
                .and(bln(true, "B"))
                .or(bln(true, "C"));
        System.out.println("A,true AND B,true OR C,true: " + aAndBorC);

        BooleanExpression5 aAndB_or_cAndD = bln(
                bln(true, "A").and(
                        bln(true, "B"))
        ).or(bln(true, "C").and(
                bln(true, "D")));

        System.out.println("(A,true AND B,true) OR (C,true and D,true): " + aAndB_or_cAndD);

        BooleanExpression5 aAndBorCAndD =
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

        BooleanExpression5 isTheRealPhil = bln((PenguinFacts f) -> f.name.equals("Phil"), "Name is Phil")
                .and((PenguinFacts f) -> f.age == 2, "Age is 2, because the real Phil is 2")
                .and(not((PenguinFacts f) -> f.favoriteColor.equals("Red"), "Favorite color is red"));

        assertFalse(isTheRealPhil.evaluate(new PenguinFacts("Annie", 2, "Red")));
        assertFalse(isTheRealPhil.evaluate(new PenguinFacts("Phil", 3, "Red")));
        assertTrue(isTheRealPhil.evaluate(new PenguinFacts("Phil", 2, "Blue")));

        System.out.println("done");
    }

    @Test
    public void testVisitor() {

        BooleanExpression5 expression1 = and(
                bln(f -> true, "A"),
                bln(f -> false, "B"),
                or(
                        bln(f -> false, "C"),
                        bln(f -> true, "D")
                )
        );

        BooleanExpression5 expression3 = or(
                bln(f -> true, "A"),
                bln(f -> false, "B"),
                not(and(
                        bln(f -> false, "C"),
                        bln(f -> true, "D")
                ))
        );


        BooleanExpression5 expression2 = bln(
                bln(f -> true, "A")
                        .and(f -> false, "B")
        ).or(
                bln(f -> true, "C")
                        .and(f -> false, "D")
        ).or(
                bln(false, "E")
                        .and(false, "F")
                        .and(false, "G")
        ).or(
                bln(false, "H")
                        .or(false, "I")
                        .or(true, "J")
                        .or(false, "K")
                        .or(true, "L")
        );

        expression2.visit(new BooleanExpression5.BooleanExpressionVisitor() {
            @Override
            public void visitBooleanWrapper(BooleanExpression5.BooleanWrapper booleanWrapper) {
                System.out.println("booleanWrapper = " + booleanWrapper);
            }

            @Override
            public void visitBooleanAnd(BooleanExpression5.BooleanAnd booleanAnd) {
                System.out.println("booleanAnd = " + booleanAnd);
            }

            @Override
            public void visitBooleanOr(BooleanExpression5.BooleanOr booleanOr) {
                System.out.println("booleanOr = " + booleanOr);
            }

            @Override
            public void visitBooleanNot(BooleanExpression5.BooleanNot booleanNot) {
                System.out.println("booleanNot = " + booleanNot);
            }

            @Override
            public void visitBooleanValue(BooleanExpression5.BooleanValue booleanValue) {
                System.out.println("booleanValue = " + booleanValue);
            }

            @Override
            public void visitBooleanFunction(BooleanExpression5.BooleanFunction booleanFunction) {
                System.out.println("booleanFunction = " + booleanFunction);
            }
        });
    }
}
