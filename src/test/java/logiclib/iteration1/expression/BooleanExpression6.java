package logiclib.iteration1.expression;

import java.util.Objects;
import java.util.StringJoiner;
import java.util.function.Function;

public abstract class BooleanExpression6 {

    private String name;
    private Boolean value;
    private Throwable exception;  // if exception was encountered during evaluation
    private boolean evaluated;
    private Object userData;

    // Boolean Expression Creators

    public static BooleanExpression6 bln(Boolean value, String name) {
        return new BooleanValue(value, name);
    }

    public static <F> BooleanExpression6 bln(Function<F, Boolean> bln, String name) {
        return new BooleanFunction((Function) bln, name);
    }

    public static BooleanExpression6 bln(BooleanExpression6 other) {
        return new BooleanWrapper(other);
    }

    // Boolean NOT Expression Creators

    public static BooleanExpression6 not(Boolean value, String name) {
        return new BooleanNot(new BooleanValue(value, name));
    }

    public static <F> BooleanExpression6 not(Function<F, Boolean> function, String name) {
        return new BooleanNot(new BooleanFunction((Function) function, name));
    }

    public static BooleanExpression6 not(BooleanExpression6 other) {
        return new BooleanNot(other);
    }

    // Boolean AND Expression Creators

    public BooleanExpression6 and(Boolean value, String name) {
        return new BooleanAnd(this, new BooleanValue(value, name));
    }

    public <F> BooleanExpression6 and(Function<F, Boolean> function, String name) {
        return new BooleanAnd(this, new BooleanFunction((Function) function, name));
    }

    public BooleanExpression6 and(BooleanExpression6 other) {
        return new BooleanAnd(this, other);
    }

    public static BooleanExpression6 and(BooleanExpression6... expressions) {
        if (expressions.length == 0)
            throw new IllegalArgumentException("Must have at least 1 expression");

        return new BooleanAnd(expressions);
    }

    // Boolean OR Expression Creators

    public BooleanExpression6 or(Boolean value, String name) {
        return new BooleanOr(this, new BooleanValue(value, name));
    }

    public <F> BooleanExpression6 or(Function<F, Boolean> function, String name) {
        return new BooleanOr(this, new BooleanFunction((Function) function, name));
    }

    public BooleanExpression6 or(BooleanExpression6 other) {
        return new BooleanOr(this, other);
    }

    public static BooleanExpression6 or(BooleanExpression6... expressions) {
        if (expressions.length == 0)
            throw new IllegalArgumentException("Must have at least 1 expression");

        return new BooleanOr(expressions);
    }

    /**
     * Evaluate the boolean expression based on the given facts.
     * This method is not thread safe.
     *
     * @param facts the facts to evaluate
     * @return the value of the expression after it has been evaluated against the given facts.
     */
    public Boolean evaluate(Object facts) {
        if (isEvaluated()) {
            // evaluate again
            evaluated = false;
            value = null;
            exception = null;
        }

        try {
            value = doEvaluate(facts);  // call the specific boolean expression's implementation.
            evaluated = true;
            return value;
        } catch (Throwable e) {
            exception = e;
            throw new IllegalStateException("Error evaluating boolean expression", e);
        }
    }

    /**
     * Derive the actual boolean value of this expression
     *
     * @param facts the facts to use in the derivation
     * @return the boolean value of this expression
     */
    protected abstract Boolean doEvaluate(Object facts);

    /**
     * Visit this boolean expression.
     *
     * @param visitor the visitor to invoke callback methods upon.
     */
    public abstract void visit(BooleanExpressionVisitor visitor);

    public String getName() {
        return name;
    }

    protected void setName(String name) {
        this.name = name;
    }

    public Boolean getValue() {
        if (!isEvaluated())
            throw new IllegalStateException("Boolean value not available because expression has not been evaluated");
        return value;
    }

    public boolean isEvaluated() {
        return evaluated;
    }

    public Throwable getException() {
        return exception;
    }

    public Object getUserData() {
        return userData;
    }

    public void setUserData(Object userData) {
        this.userData = userData;
    }

    public static class BooleanValue extends BooleanExpression6 {
        private final boolean value;

        public BooleanValue(boolean value, String name) {
            setName(name);
            this.value = value;
        }

        @Override
        public String toString() {
            return "BooleanValue{" + getName() + "=" + value + '}';
        }

        @Override
        protected Boolean doEvaluate(Object facts) {
            return value;
        }

        @Override
        public void visit(BooleanExpressionVisitor visitor) {
            visitor.visitBooleanValue(this);
        }
    }

    public static class BooleanFunction extends BooleanExpression6 {
        private final Function<Object, Boolean> booleanFunction;

        public BooleanFunction(Function<Object, Boolean> booleanFunction, String name) {
            setName(name);
            this.booleanFunction = Objects.requireNonNull(booleanFunction, "Function cannot be null");
        }

        @Override
        public String toString() {
            return "BooleanFunction{" + getName() + "=" + booleanFunction + '}';
        }

        @Override
        protected Boolean doEvaluate(Object facts) {
            return booleanFunction.apply(facts);
        }

        @Override
        public void visit(BooleanExpressionVisitor visitor) {
            visitor.visitBooleanFunction(this);
        }
    }

    public static class BooleanWrapper extends BooleanExpression6 {
        private final BooleanExpression6 wrapped;

        public BooleanWrapper(BooleanExpression6 wrapped) {
            setName(wrapped.getName());
            this.wrapped = Objects.requireNonNull(wrapped, "Expression cannot be null");
        }

        public BooleanExpression6 getWrapped() {
            return wrapped;
        }

        @Override
        public String toString() {
            return wrapped.toString();
        }

        @Override
        protected Boolean doEvaluate(Object facts) {
            return wrapped.doEvaluate(facts);
        }

        @Override
        public void visit(BooleanExpressionVisitor visitor) {
            visitor.visitBooleanWrapper(this);
            wrapped.visit(visitor);
        }
    }

    public static class BooleanAnd extends BooleanExpression6 {

        private final BooleanExpression6[] args;

        public BooleanAnd(BooleanExpression6... args) {
            setName(buildName(args));
            this.args = args;
        }

        @Override
        protected Boolean doEvaluate(Object facts) {
            for (BooleanExpression6 arg : args) {
                Boolean value = arg.evaluate(facts);
                if (!value)
                    return false;
            }

            return true;
        }

        private static String buildName(BooleanExpression6... args) {
            StringJoiner joiner = new StringJoiner(" AND ", "(", ")");
            for (BooleanExpression6 arg : args) {
                joiner.add(arg.getName());
            }
            return joiner.toString();
        }

        public BooleanExpression6[] getArgs() {
            return args;
        }

        @Override
        public String toString() {
            return getName();
        }

        public void visit(BooleanExpressionVisitor visitor) {
            visitor.visitBooleanAnd(this);
            for (BooleanExpression6 arg : args) {
                arg.visit(visitor);
            }
        }
    }

    public static class BooleanOr extends BooleanExpression6 {

        private final BooleanExpression6[] args;

        public BooleanOr(BooleanExpression6... args) {
            setName(buildName(args));
            this.args = args;
        }

        @Override
        protected Boolean doEvaluate(Object facts) {
            for (BooleanExpression6 arg : args) {
                Boolean value = arg.evaluate(facts);
                if (value)
                    return true;
            }

            return false;
        }

        private static String buildName(BooleanExpression6... args) {
            StringJoiner joiner = new StringJoiner(" OR ", "(", ")");
            for (BooleanExpression6 arg : args) {
                joiner.add(arg.getName());
            }
            return joiner.toString();
        }

        public BooleanExpression6[] getArgs() {
            return args;
        }

        @Override
        public String toString() {
            return getName();
        }

        public void visit(BooleanExpressionVisitor visitor) {
            visitor.visitBooleanOr(this);
            for (BooleanExpression6 arg : args) {
                arg.visit(visitor);
            }
        }
    }

    public static class BooleanNot extends BooleanExpression6 {
        private final BooleanExpression6 arg;

        public BooleanNot(BooleanExpression6 arg) {
            setName("NOT " + arg.getName());
            this.arg = arg;
        }

        @Override
        protected Boolean doEvaluate(Object facts) {
            return !arg.evaluate(facts);
        }

        @Override
        public String toString() {
            return getName();
        }

        public BooleanExpression6 getArg() {
            return arg;
        }

        @Override
        public void visit(BooleanExpressionVisitor visitor) {
            visitor.visitBooleanNot(this);
            arg.visit(visitor);
        }
    }

    public interface BooleanExpressionVisitor {
        default void visitBooleanWrapper(BooleanWrapper booleanWrapper) {
        }

        default void visitBooleanAnd(BooleanAnd booleanAnd) {
        }

        default void visitBooleanOr(BooleanOr booleanOr) {
        }

        default void visitBooleanNot(BooleanNot booleanNot) {
        }

        default void visitBooleanValue(BooleanValue booleanValue) {
        }

        default void visitBooleanFunction(BooleanFunction booleanFunction) {
        }
    }
}
