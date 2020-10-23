package test

def cases1 = [
        [
                "a": "valA",
        ],
        [
                "b": "valB",
        ],
]

def cases2 = [
        [
                "field2": "val2"
        ],
        [
                "field2": "val3"
        ],
        [
                "field2": "val4"
        ]
]

def cases3 = [
        [
                "field3": "val5"
        ]
]


// try to express cases1 AND cases2

def expressions = [cases1, cases2, cases3]

List<Map<String, Object>> workingCases = new ArrayList<>();
for (List<Map<String, Object>> expression : expressions) {
    List<Map<String, Object>> expressionCases = expression  // would derive this, the list here is an IMPLICIT OR


    if (workingCases.size() == 0) {
        // no working cases yet
        // first expression
        workingCases.addAll(expressionCases)
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

workingCases.each {println it
}
println "done"