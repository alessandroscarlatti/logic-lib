package logiclib.iteration1.expression;

import org.junit.Test;

import static logiclib.iteration1.expression.BooleanExpression2.bln;

public class BooleanExpression2Test {

    @Test
    public void testBooleanExpression() {
        boolean[][] testCases = new boolean[][]{
            {true, true, true, true},  // true
            {false, true, true, true},  // true
            {false, false, false, false},  // false
            {true, true, true, false},  // true
            {false, false, false, false}, // false
            {false, false, true, false}, // true
            {false, false, true, true} // true
        };

        for (int i = 0; i < testCases.length; i++) {
            boolean a = testCases[i][0];
            boolean b = testCases[i][1];
            boolean c = testCases[i][2];
            boolean d = testCases[i][3];
            boolean expected = (a && b) || (c && d);
            boolean actual = bln(bln(() -> a)
            .and(bln(() -> b))).or(
                    bln(() -> c).and(bln(() -> d))
            ).evaluate();

            if (expected != actual) {
                System.err.printf("Expected (%s AND %s) OR (%s AND %s) to return %s, but was %s%n", a, b, c, d, expected, actual);
            }
        }

        System.out.println("passed");
    }
}
