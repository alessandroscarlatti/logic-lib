package logiclib.iteration1.control;

import logiclib.iteration1.expression.BooleanExpression5;
import org.codehaus.groovy.ast.stmt.IfStatement;

import java.util.function.Consumer;

public class Statement {

    public static class SimpleStatement {
        public static Statement statement(Consumer<Object> statement, String name) {
            return new Statement();
        }
    }

    public static class IfStatement extends Statement {
        private BooleanExpression5 condition;
        private boolean evaluated;
        private Statement selectedStatement;
        private Statement thenStatement;
        private Statement elseStatement;
        private Throwable exception;
        private String name;

        public static IfStatement ifStatement(BooleanExpression5 condition) {
            return new IfStatement();
        }

        public static IfStatement ifStatement(BooleanExpression5 condition, Statement...statements) {
            return new IfStatement();
        }

        public static BooleanExpression5 condition(BooleanExpression5 expression) {
            return expression;
        }

        public static Statement thenStatement(Statement statement) {
            return new Statement();
        }

        public static Statement elseStatement(Statement statement) {
            return new Statement();
        }

        public static IfStatement elseIfStatement(BooleanExpression5 condition) {
            return new IfStatement();
        }

        public void evaluate(Object facts) {
            if (isEvaluated()) {
                // evaluate again
                evaluated = false;
                selectedStatement = null;
                exception = null;
            }

            try {
                if (condition.evaluate(facts)) {
                    selectedStatement = thenStatement;
                } else {
                    selectedStatement = elseStatement;
                }

//                selectedStatement.evaluate(facts);
                evaluated = true;
            } catch (Throwable e) {
                exception = e;
                throw new IllegalStateException("Error evaluating if statement", e);
            }
        }

        public boolean isEvaluated() {
            return evaluated;
        }

        public BooleanExpression5 getCondition() {
            return condition;
        }

        public void setCondition(BooleanExpression5 condition) {
            this.condition = condition;
        }
    }


}
