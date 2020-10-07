package logiclib.iteration1;

import org.junit.Test;

import java.util.function.Function;
import java.util.function.Supplier;

import static logiclib.iteration1.BooleanExpression.*;
import static logiclib.iteration1.BooleanExpression.BlnOperator.*;
import static logiclib.iteration1.BooleanExpression.not;
import static org.junit.Assert.*;

public class BooleanExpressionTest {

    @Test
    public void testBooleanTrue() {
        BooleanExpression booleanExpression = new BooleanExpression(() -> true);
        assertTrue(booleanExpression.evaluate());
        assertTrue(booleanExpression.getValue());
    }

    @Test
    public void testBooleanFalse() {
        BooleanExpression booleanExpression = new BooleanExpression(() -> false);
        assertFalse(booleanExpression.evaluate());
        assertFalse(booleanExpression.getValue());
    }

    @Test
    public void testBooleanException() {
        BooleanExpression booleanExpression = new BooleanExpression(() -> {
            throw new IllegalStateException("TestException");
        });
        try {
            booleanExpression.evaluate();
            fail("Expected exception was not thrown.");
        } catch (Exception e) {
            e.printStackTrace();
        }

        assertEquals("TestException", booleanExpression.getException().getMessage());
    }

    @Test
    public void testBooleanGetValueEarlyException() {
        BooleanExpression booleanExpression = new BooleanExpression(() -> true);
        try {
            // did not evaluate the expression
            booleanExpression.getValue();
            fail("Expected exception was not thrown.");
        } catch (Exception e) {
            e.printStackTrace();
            assertTrue(e instanceof IllegalStateException);
        }
    }

    @Test
    public void testBooleanEvaluateTwiceException() {
        BooleanExpression booleanExpression = new BooleanExpression(() -> true);
        try {
            // evaluating twice is not allowed
            booleanExpression.evaluate();
            booleanExpression.evaluate();
            fail("Expected exception was not thrown.");
        } catch (Exception e) {
            e.printStackTrace();
            assertTrue(e instanceof IllegalStateException);
        }
    }

    @Test
    public void testBoolean_AandB_true() {
        BooleanExpression a = new BooleanExpression(() -> true);
        BooleanExpression b = new BooleanExpression(() -> true);
        BooleanExpression aAndB = new BooleanExpression(() -> a.evaluate() && b.evaluate());
        aAndB.evaluate();
        assertTrue(aAndB.getValue());
    }

    @Test
    public void testBoolean_AandB_false() {
        BooleanExpression a = new BooleanExpression(() -> true);
        BooleanExpression b = new BooleanExpression(() -> false);
        BooleanExpression aAndB = new BooleanExpression(() -> a.evaluate() && b.evaluate());
        aAndB.evaluate();
        assertFalse(aAndB.getValue());
    }

    @Test
    public void testBooleanReusable_Syntax1() {
        Supplier<BooleanExpression> bln = () -> {
            return new BooleanExpression(() -> true);
        };

        for (int i = 0; i < 3; i++) {
            BooleanExpression booleanExpression = bln.get();
            booleanExpression.evaluate();
            assertTrue(booleanExpression.getValue());
        }
    }

    @Test
    public void testBooleanReusable_Syntax2() {
        Function<Supplier<Boolean>, BooleanExpression> bln = (supplier) -> {
            return new BooleanExpression(supplier);
        };

        for (int i = 0; i < 3; i++) {
            BooleanExpression booleanExpression = bln.apply(() -> true);
            booleanExpression.evaluate();
            assertTrue(booleanExpression.getValue());
        }
    }

    @Test
    public void testBooleanReusable_Syntax2_AandB() {
        Function<Supplier<Boolean>, BooleanExpression> bln = (supplier) -> {
            return new BooleanExpression(supplier);
        };

        for (int i = 0; i < 3; i++) {
            BooleanExpression a = bln.apply(() -> true);
            BooleanExpression b = bln.apply(() -> true);
            BooleanExpression aAndB = bln.apply(() -> a.evaluate() && b.evaluate());
            aAndB.evaluate();
            assertTrue(aAndB.getValue());
        }
    }

    @Test
    public void testBooleanReusable_Syntax3_AandB() {

        class BlnFactory {
            BooleanExpression aAndB(Boolean a, Boolean b) {
                BooleanExpression blnA = new BooleanExpression(() -> a);
                BooleanExpression blnB = new BooleanExpression(() -> b);
                return new BooleanExpression(() -> blnA.evaluate() && blnB.evaluate());
            }
        }

        BlnFactory blnFactory = new BlnFactory();

        assertTrue(blnFactory.aAndB(true, true).evaluate());
        assertFalse(blnFactory.aAndB(true, false).evaluate());
    }

    @Test
    public void testBooleanReusable_GroupExpression_AandB() {

        // somehow need some kind of a .and() method...
        // would a BooleanGroupExpression be a different class?
        // or would a single BooleanExpression class cover both cases?

        class BlnFactory {
            BooleanExpression aAndB(Boolean a, Boolean b) {
                BooleanExpression blnA = new BooleanExpression(() -> a);
                BooleanExpression blnB = new BooleanExpression(() -> b);

                return bln(blnA, AND, blnB);
            }
        }

        BlnFactory blnFactory = new BlnFactory();

        assertTrue(blnFactory.aAndB(true, true).evaluate());
        assertFalse(blnFactory.aAndB(true, false).evaluate());
    }

    @Test
    public void testBooleanReusable_GroupExpression_AorB() {

        // somehow need some kind of a .and() method...
        // would a BooleanGroupExpression be a different class?
        // or would a single BooleanExpression class cover both cases?

        class BlnFactory {
            BooleanExpression aOrB(Boolean a, Boolean b) {
                BooleanExpression blnA = new BooleanExpression(() -> a);
                BooleanExpression blnB = new BooleanExpression(() -> b);

                return bln(blnA, OR, blnB);
            }
        }

        BlnFactory blnFactory = new BlnFactory();

        assertTrue(blnFactory.aOrB(true, false).evaluate());
        assertFalse(blnFactory.aOrB(false, false).evaluate());
    }

    @Test
    public void testBooleanReusable_GroupExpression_AandBorC() {
        assertTrue(bln(true, AND, true, OR, true).evaluate());
        assertTrue(bln(true, AND, true, OR, false).evaluate());

        assertTrue(bln(false, AND, false, OR, true).evaluate());
        assertTrue(bln(false, AND, true, OR, true).evaluate());

        assertFalse(bln(false, AND, false, OR, false).evaluate());
        assertFalse(bln(false, AND, true, OR, false).evaluate());
    }

    @Test
    public void testBooleanReusable_GroupExpression_NotA() {
        assertTrue(bln(NOT, false).evaluate());
        assertFalse(bln(NOT, true).evaluate());
    }

    @Test
    public void testBooleanReusable_GroupExpression_AandNotB() {
        assertTrue(bln(true, AND, NOT, false).evaluate());
        assertFalse(bln(true, AND, NOT, true).evaluate());
        assertFalse(bln(false, AND, NOT, false).evaluate());
        assertFalse(bln(false, AND, NOT, true).evaluate());
    }

    @Test
    public void testBooleanReusable_GroupExpression_AandNotBandNotC() {
        assertTrue(bln(true, AND, NOT, false, AND, NOT, false).evaluate());
        assertFalse(bln(true, AND, NOT, false, AND, NOT, true).evaluate());
        assertFalse(bln(false, AND, NOT, false, AND, NOT, true).evaluate());
        assertFalse(bln(false, AND, NOT, true, AND, NOT, true).evaluate());
    }

    @Test
    public void testBooleanReusable_GroupExpression_NotAandNotBandNotC() {
        assertTrue(bln(NOT, false, AND, NOT, false, AND, NOT, false).evaluate());
        assertFalse(bln(NOT, false, AND, NOT, true, AND, NOT, true).evaluate());
    }

    @Test
    public void testBoolean_GroupExpression_Chaining_AandB() {
        assertTrue(bln(true).and(bln(true)).evaluate());
        assertFalse(bln(true).and(bln(false)).evaluate());
        assertFalse(bln(false).and(bln(false)).evaluate());
    }

    @Test
    public void testBoolean_GroupExpression_Chaining_AandBandC() {
        assertTrue(bln(true)
                .and(bln(true)
                        .and(bln(true))).evaluate());
        assertFalse(bln(true)
                .and(bln(true)
                        .and(bln(false))).evaluate());
    }

    @Test
    public void testBoolean_GroupExpression_Chaining_AandBorC() {
        assertTrue(bln(true)
                .and(bln(true)
                        .or(bln(true))).evaluate());
        assertFalse(bln(true)
                .and(bln(false)
                        .or(bln(false))).evaluate());
    }

    @Test
    public void testBoolean_GroupExpression_Chaining_AandNotB() {
        BooleanExpression expr1 = bln(true, "A")
                .and(not(bln(false, "B")));

        BooleanExpression expr2 = bln(true, "A")
                .and(not(bln(true, "B")));

        System.out.println("A = true AND B = false : " + expr1);
        System.out.println("A = true AND B = true : " + expr2);

        assertTrue(expr1.evaluate());
        assertFalse(expr2.evaluate());
    }

    @Test
    public void testBooleanToString() {
        BooleanExpression a = bln(true, "A");
        System.out.println("A,true: " + a);

        BooleanExpression aAndB = bln(true, "A")
                .and(bln(true, "B"));
        System.out.println("A,true AND B,true: " + aAndB);

        BooleanExpression aAndBorC = bln(true, "A")
                .and(bln(true, "B"))
                .or(bln(true, "C"));
        System.out.println("A,true AND B,true OR C,true: " + aAndBorC);

        BooleanExpression aAndB_or_cAndD = bln(
                bln(true, "A")
                        .and(bln(true, "B"))
        ).or(bln(
                bln(true, "C")
                        .and(bln(true, "D"))
        ));

        System.out.println("(A,true AND B,true) OR (C,true and D,true): " + aAndB_or_cAndD);

    }


}