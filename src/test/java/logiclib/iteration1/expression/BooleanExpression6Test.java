package logiclib.iteration1.expression;

import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BooleanExpression6Test {

    @Test
    public void testCaseRepresentation() {
        BooleanExpression6 bln = BooleanExpression6.bln(true, "asdf");


    }

    private static List<Map<String, Object>> getCases(BooleanExpression6 bln) {
        List<Map<String, Object>> cases = new ArrayList<>();

        if (bln instanceof BooleanExpression6.BooleanAnd) {

        }

        if (bln instanceof BooleanExpression6.BooleanOr) {

        }

        if (bln instanceof BooleanExpression6.BooleanValue) {

        }

        return cases;
    }

    private static List<Map<String, Object>> getBooleanOrCases(List<BooleanExpression6> expressions) {
        List<Map<String, Object>> cases = new ArrayList<>();
        for (BooleanExpression6 expression : expressions) {
            List<Map<String, Object>> casesForExpression = getCases(expression);
            cases.addAll(casesForExpression);
        }
        return cases;
    }

    private static List<Map<String, Object>> getBooleanAndCases(List<BooleanExpression6> expressions) {
        List<Map<String, Object>> workingCases = new ArrayList<>();
        for (BooleanExpression6 expression : expressions) {
            List<Map<String, Object>> expressionCases = getCases(expression);  // the list here is an IMPLICIT OR

            if (workingCases.size() == 0) {
                // no working cases yet
                // first expression
                workingCases.addAll(expressionCases);
            } else {
                // not the first expression so combine stuff
                List<Map<String, Object>> combinedCasesForExpression = new ArrayList<>();
                for (Map<String, Object> workingCase : workingCases) {
                    for (Map<String, Object> expressionCase : expressionCases) {
                        // create a new copy
                        Map<String, Object> newCase = new HashMap<>(workingCase);
                        newCase.putAll(expressionCase);  // this is where overriding would take place
                        combinedCasesForExpression.add(newCase);
                    }
                }
                workingCases = combinedCasesForExpression;
            }
        }
        return workingCases;
    }

    // each list here represents an implicit AND
    // multiple items in the actual AND boolean would be accomplished by each inheriting from the other
    private static void performOverrideForCaseOr(List<Map<String, Object>> parent, List<Map<String, Object>> child) {
        // combining an or is easy...
        // we just combine the lists, right???
        //
        // concatenate A + B, then (A+B) + C
        //
        // the only optimization here is suppressing any duplicate cases
    }

    private static String computeOverrideForParameter(String paramName, String parentValue, String childValue) {
        // this is where the values would have to be ranked by inclusivity
        // OR we assume that the child is always more inclusive than the parent
        // AND that the child is ACTUALLY INCLUDED inside the parent

        // OR if the same param name is present in the parent and the child
        // this is where logic can determine what value gets displayed...
        // eg, if the parent is an actual value,
        // but the child is a default value,
        // this logic can detect that and ignore the default value

        // by default return the child value
        return childValue;

        // now...would it ever be the case that when presented with this kind of an override,
        // we would actually add MORE param names to the case?
        // my first response is probably possible, yes...
        // but then, probably would be very complicated to code for that
    }
}


