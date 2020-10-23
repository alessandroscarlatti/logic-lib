package test

import logiclib.iteration1.expression.BooleanExpression6

import static logiclib.iteration1.expression.BooleanExpression6.and
import static logiclib.iteration1.expression.BooleanExpression6.bln
import static logiclib.iteration1.expression.BooleanExpression6.or

class Test2 {

    static void main(String[] args) {
        def casesAnd1 = [
                [
                        "a": "a1",
                ],
                [
                        "a": "a2",
                ],
        ]

        def casesAnd2 = [
                [
                        "b": "b1"
                ],
                [
                        "b": "b2"
                ],
                [
                        "b": "b3"
                ]
        ]

        def casesOr = [
                [
                        "field3": "val5"
                ]
        ]

        BooleanExpression6 bln = and(
                and(bln(true, "a"), bln(true, "b"), bln(true, "c")),
                and(bln(true, "d")),
                or(
                        and(bln(true, "d1"), bln(true, "e1"), bln(true, "f1")),
                        and(bln(true, "d2"), bln(true, "e2"), bln(true, "f2")),
                        and(bln(true, "d3"), bln(true, "e3")),
                )
        )

        List<Map<String, Object>> cases = getCases(bln)

        cases.each { println it }
    }

    private static List<Map<String, Object>> getCases(BooleanExpression6 bln) {
        List<Map<String, Object>> cases = new ArrayList<>();

        if (bln instanceof BooleanExpression6.BooleanAnd) {
            return getBooleanAndCases(((BooleanExpression6.BooleanAnd) bln).args.toList())
        }

        if (bln instanceof BooleanExpression6.BooleanOr) {
            return getBooleanOrCases(((BooleanExpression6.BooleanOr) bln).args.toList())
        }

        if (bln instanceof BooleanExpression6.BooleanValue) {
            return [
                    ["${bln.name}": "${bln.name}.value"]
            ]
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
}
