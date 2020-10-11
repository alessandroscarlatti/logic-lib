package logiclib.iteration1;

import java.util.Objects;
import java.util.function.Function;

public class BooleanExpression3 {

    private String name;
    private Boolean value;
    private Throwable exception;  // if exception was encountered during evaluation
    private boolean evaluated;
    private Function<Object, Boolean> supplierFunction;
    private Object userData;

    public BooleanExpression3(Function<Object, Boolean> supplierFunction, String name) {
        Objects.requireNonNull(supplierFunction, "Boolean supplier cannot be null");
        this.supplierFunction = supplierFunction;
        this.name = name != null ? name : "<name?>";
    }

    public static BooleanExpression3 bln(Boolean value, String name) {
        return bln(new RawBlnValue(value), name);
    }

    public static <F> BooleanExpression3 bln(Function<F, Boolean> bln, String name) {
        return new BooleanExpression3((Function) bln, name);
    }

    public static BooleanExpression3 bln(BooleanExpression3 other) {
        return bln(new BooleanExpressionBooleanSupplier(other), other.getName());
    }

    public BooleanExpression3 and(Boolean value, String name) {
        return and(bln(value, name));
    }

    public <F> BooleanExpression3 and(Function<F, Boolean> function, String name) {
        return and(bln(function, name));
    }

    public BooleanExpression3 and(BooleanExpression3 other) {
        return bln(new BlnAnd(this, other), BlnAnd.getName(this, other));
    }

    public BooleanExpression3 or(Boolean value, String name) {
        return or(bln(value, name));
    }

    public <F> BooleanExpression3 or(Function<F, Boolean> function, String name) {
        return or(bln(function, name));
    }

    public BooleanExpression3 or(BooleanExpression3 other) {
        return bln(new BlnOr(this, other), BlnOr.getName(this, other));
    }

    public static BooleanExpression3 not(Boolean value, String name) {
        return not(bln(value, name));
    }

    public static <F> BooleanExpression3 not(Function<F, Boolean> bln, String name) {
        return not(bln(bln, name));
    }

    public static BooleanExpression3 not(BooleanExpression3 other) {
        return bln(new BlnNot(other), BlnNot.getName(other));
    }

    public Boolean evaluate(Object facts) {
        if (isEvaluated()) {
            // evaluate again
            evaluated = false;
            value = null;
            exception = null;
        }

        try {
            value = supplierFunction.apply(facts);
            evaluated = true;
            return value;
        } catch (Throwable e) {
            exception = e;
            throw new IllegalStateException("Error evaluating boolean expression", e);
        }
    }

    @Override
    public String toString() {
        return "BooleanExpression {"
                + name +
                '}';
    }

    public void visit(BlnExpressionVisitor visitor) {
        visitor.visitBooleanExpression(this);

        if (supplierFunction instanceof BlnAnd) {
            BlnAnd blnAnd = ((BlnAnd) supplierFunction);
            visitor.visitBooleanAnd(this, blnAnd.arg1, blnAnd.arg2);
            blnAnd.visit(visitor);
        } else if (supplierFunction instanceof BlnOr) {
            BlnOr blnOr = ((BlnOr) supplierFunction);
            visitor.visitBooleanOr(this, blnOr.arg1, blnOr.arg2);
            blnOr.visit(visitor);
        } else if (supplierFunction instanceof BooleanExpression3) {
            ((BooleanExpression3) supplierFunction).visit(visitor);
        } else if (supplierFunction instanceof BooleanExpressionBooleanSupplier) {
            // this is a boolean group
            ((BooleanExpressionBooleanSupplier) supplierFunction).expr.visit(visitor);
        } else {
            // this wound up capturing a boolean group...(A AND B)
            visitor.visitBooleanLeaf(this);
        }
    }

    public String getName() {
        return name;
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

    private static class RawBlnValue implements Function<Object, Boolean> {
        private final boolean value;

        public RawBlnValue(boolean value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "BooleanValue{" + value + '}';
        }

        @Override
        public Boolean apply(Object facts) {
            return value;
        }
    }

    private static class BooleanExpressionBooleanSupplier implements Function<Object, Boolean> {
        private final BooleanExpression3 expr;

        public BooleanExpressionBooleanSupplier(BooleanExpression3 expr) {
            Objects.requireNonNull(expr, "Expression cannot be null");
            this.expr = expr;
        }

        @Override
        public String toString() {
            return expr.toString();
        }

        @Override
        public Boolean apply(Object facts) {
            return expr.evaluate(facts);
        }
    }

    public static class BlnAnd implements Function<Object, Boolean> {

        private final BooleanExpression3 arg1;
        private final BooleanExpression3 arg2;

        public BlnAnd(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            this.arg1 = arg1;
            this.arg2 = arg2;
        }

        @Override
        public Boolean apply(Object facts) {
            return arg1.evaluate(facts) && arg2.evaluate(facts);
        }

        @Override
        public String toString() {
            return getName(arg1, arg2);
        }

        public static String getName(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            return "(" + arg1.getName() + " AND " + arg2.getName() + ")";
        }

        public void visit(BlnExpressionVisitor visitor) {
            visitor.visitBooleanAnd(arg1, arg2);
            arg1.visit(visitor);
            arg2.visit(visitor);
        }
    }

    public static class BlnOr implements Function<Object, Boolean> {

        private final BooleanExpression3 arg1;
        private final BooleanExpression3 arg2;

        public BlnOr(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            this.arg1 = arg1;
            this.arg2 = arg2;
        }

        @Override
        public Boolean apply(Object facts) {
            return arg1.evaluate(facts) || arg2.evaluate(facts);
        }

        @Override
        public String toString() {
            return getName(arg1, arg2);
        }

        public static String getName(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            return "(" + arg1.getName() + " OR " + arg2.getName() + ")";
        }

        public void visit(BlnExpressionVisitor visitor) {
            visitor.visitBooleanOr(arg1, arg2);
            arg1.visit(visitor);
            arg2.visit(visitor);
        }
    }

    public static class BlnNot implements Function<Object, Boolean> {
        private final BooleanExpression3 arg;

        public BlnNot(BooleanExpression3 arg) {
            this.arg = arg;
        }

        @Override
        public Boolean apply(Object facts) {
            return !arg.evaluate(facts);
        }

        @Override
        public String toString() {
            return getName(arg);
        }

        public static String getName(BooleanExpression3 arg) {
            return "NOT " + arg.getName() + ")";
        }
    }

    public interface BlnExpressionVisitor {
        default void visitBooleanExpression(BooleanExpression3 expression) {
        }

        default void visitBooleanAnd(BooleanExpression3 arg1, BooleanExpression3 arg2) {
        }

        default void visitBooleanOr(BooleanExpression3 arg1, BooleanExpression3 arg2) {
        }

        default void visitBooleanAnd(BooleanExpression3 andExpression, BooleanExpression3 arg1, BooleanExpression3 arg2) {
        }

        default void visitBooleanOr(BooleanExpression3 orExpression, BooleanExpression3 arg1, BooleanExpression3 arg2) {
        }

        default void visitBooleanLeaf(BooleanExpression3 expression) {
        }
    }
}
