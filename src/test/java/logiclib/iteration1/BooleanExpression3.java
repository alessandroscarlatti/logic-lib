package logiclib.iteration1;

import java.util.Objects;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

public class BooleanExpression3 {

    private String name;
    private Boolean value;
    private Throwable exception;  // if exception was encountered during evaluation
    private boolean evaluated;
    private Supplier<Boolean> supplier;
    private Function<Object, Boolean> supplierFunction;
    private Object userData;

    public BooleanExpression3(Supplier<Boolean> supplier, String name) {
        Objects.requireNonNull(supplier, "Boolean supplier cannot be null");
        this.supplier = supplier;
        this.name = name;
    }

    public BooleanExpression3(Supplier<Boolean> supplier) {
        Objects.requireNonNull(supplier, "Boolean supplier cannot be null");
        this.supplier = supplier;
    }

    public BooleanExpression3(Function<Object, Boolean> supplierFunction) {
        Objects.requireNonNull(supplierFunction, "Boolean supplier cannot be null");
        this.supplierFunction = supplierFunction;
    }

    @Override
    public String toString() {
        return "BooleanExpression{" +
                "name='" + name + '\'' +
                ", supplier=" + supplier +
                '}';
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

    public BooleanExpression3 and(BooleanExpression3 other) {
        return BlnOperator.AND.apply(this, other);
    }

    public BooleanExpression3 and(Function<Object, Boolean> other) {
        return BlnOperator.AND.apply(this, new BooleanExpression3(other));
    }

    public BooleanExpression3 or(BooleanExpression3 other) {
        return BlnOperator.OR.apply(this, other);
    }

    public BooleanExpression3 not() {
        return BlnOperator.NOT.apply(this);
    }

    public static BooleanExpression3 not(BooleanExpression3 other) {
        return BlnOperator.NOT.apply(other);
    }

    public Boolean evaluate() {
        if (isEvaluated()) {
            // evaluate again
            evaluated = false;
            value = null;
            exception = null;
        }

        try {
            value = supplier.get();
            evaluated = true;
            return value;
        } catch (Throwable e) {
            exception = e;
            throw new IllegalStateException("Error evaluating boolean expression", e);
        }
    }

    public Boolean evaluate(Object facts) {
        return false;
    }

    public static BooleanExpression3 bln(Supplier<Boolean> bln) {
        return new BooleanExpression3(bln);
    }

    public static BooleanExpression3 bln(Function<Object, Boolean> bln) {
        return new BooleanExpression3(bln);
    }

    public static BooleanExpression3 bln(Supplier<Boolean> bln, String name) {
        return new BooleanExpression3(bln, name);
    }

    public static BooleanExpression3 bln(Boolean value) {
        return new BooleanExpression3(new RawBooleanValueSupplier(value));
    }

    public static BooleanExpression3 bln(Boolean value, String name) {
        return new BooleanExpression3(new RawBooleanValueSupplier(value), name);
    }

    public static BooleanExpression3 bln(BooleanExpression3 expr) {
        return new BooleanExpression3(new BooleanExpressionBooleanSupplier(expr), expr.getName());
    }

    public static BooleanExpression3 bln(BooleanExpression3 expr, String name) {
        return new BooleanExpression3(new BooleanExpressionBooleanSupplier(expr), name);
    }

    private static class RawBooleanValueSupplier implements Supplier<Boolean> {
        private final boolean value;

        public RawBooleanValueSupplier(boolean value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "BooleanValue{" +
                    "value=" + value +
                    '}';
        }

        @Override
        public Boolean get() {
            return value;
        }
    }

    private static class BooleanExpressionBooleanSupplier implements Supplier<Boolean> {
        private final BooleanExpression3 expr;

        public BooleanExpressionBooleanSupplier(BooleanExpression3 expr) {
            this.expr = expr;
        }

        @Override
        public String toString() {
            return "BooleanExpressionBooleanSupplier{" +
                    "expr=" + expr +
                    '}';
        }

        @Override
        public Boolean get() {
            return expr.evaluate();
        }
    }

    /**
     * Build a boolean group expression.
     *
     * @param args expressions and operators, in order, eg Expr1, Operator1, Expr2
     * @return a boolean group expression.
     */
    public static BooleanExpression3 bln(Object... args) {
        return new BooleanExpression3(() -> {
            int[] nextIndexWrapper = new int[]{0};

            BooleanExpression3 currentExpr = parseBooleanExpression(args, 0, nextIndexWrapper);

            for (int i = nextIndexWrapper[0] + 1; i < args.length; ) {
                // increment i by 2 each iteration
                try {
                    BlnBinaryOperator op = (BlnBinaryOperator) args[i];
                    BooleanExpression3 rhSide = parseBooleanExpression(args, i + 1, nextIndexWrapper);
                    currentExpr = op.apply(currentExpr, rhSide);
                    i = nextIndexWrapper[0] + 1;
                } catch (Exception e) {
                    throw new IllegalStateException("Error parsing token at index " + i, e);
                }
            }

            // evaluate whatever is left over
            return currentExpr.evaluate();


            // Original algorithm...
            // then parse that one using the old algorithm, now that we can assume only binary operators.

//            BooleanExpression currentExpr = getBooleanExpression(args[0]); // assume the first arg is a bln expression
//
//            for (int i = 1; i < args.length; i = i + 2) {
//                // increment i by 2 each iteration
//                BlnOperator op = (BlnOperator) args[i];
//                BooleanExpression rhSide = getBooleanExpression(args[i + 1]);
//                currentExpr = op.bln(currentExpr, rhSide);
//            }
//
//            // evaluate whatever is left over
//            return currentExpr.evaluate();
        });
    }

    private static BooleanExpression3 parseBooleanExpression(Object[] args, int startIndex, int[] endIndex) {
        if (args.length - startIndex < 1)
            throw new IllegalArgumentException("Cannot parse " + args.length + "  args starting at index " + startIndex +
                    ". Must parse at least 2 args, a unary operator and a boolean expression.");

        // iterate till the next non-unary operator
        // count unary operators
        int i = startIndex;
        int unaryOpCount = 0;
        while (true) {
            if (args[i] instanceof UnaryOperator)
                unaryOpCount++;
            else
                break;

            i++;
        }

        // the current token should be the boolean expression
        if (args[i] instanceof Boolean || args[i] instanceof BooleanExpression3) {
            // now we are processing the boolean expression
            BooleanExpression3 blnExpr = getBooleanExpression(args[i]);

            // apply unary operators to the nextExpr
            for (int j = 0; j < unaryOpCount; j++) {
                // reach backward in the args the specified number
                // applying unary operators from Right to Left
                blnExpr = ((BlnUnaryOperator) args[i - j - 1]).apply(blnExpr);
            }


            endIndex[0] = i;  // return which end index was used
            return blnExpr;
        } else {
            // the next token is not an expression!
            throw new IllegalStateException("Expected a boolean or boolean expression, but found " + args[i] + " at index " + i);
        }
    }

    private static BooleanExpression3 getBooleanExpression(Object arg) {
        if (arg instanceof BooleanExpression3)
            return ((BooleanExpression3) arg);
        else
            return new BooleanExpression3(() -> (Boolean) arg);
    }

    public interface BlnOperator {

        BlnBinaryOperator AND = new BlnAnd();
        BlnBinaryOperator OR = new BlnOr();
        BlnUnaryOperator NOT = new BlnNot();
    }

    public interface BlnUnaryOperator extends BlnOperator, UnaryOperator<BooleanExpression3> {
        Boolean evaluate(BooleanExpression3 arg);

        @Override
        default BooleanExpression3 apply(BooleanExpression3 arg) {
            return new BooleanExpression3(() -> evaluate(arg), toString() + " " + arg.getName());
        }
    }

    public interface BlnBinaryOperator extends BlnOperator, BinaryOperator<BooleanExpression3> {

        Boolean evaluate(BooleanExpression3 arg1, BooleanExpression3 arg2);

        @Override
        default BooleanExpression3 apply(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            return new BooleanExpression3(getBooleanSupplier(arg1, arg2), "(" + arg1.name + " " + toString() + " " + arg2.getName() + ")");
        }

        BlnBinaryOpSupplier getBooleanSupplier(BooleanExpression3 arg1, BooleanExpression3 arg2);

        abstract class BlnBinaryOpSupplier implements Supplier<Boolean> {

            protected BooleanExpression3 arg1;
            protected BooleanExpression3 arg2;

            public BlnBinaryOpSupplier(BooleanExpression3 arg1, BooleanExpression3 arg2) {
                this.arg1 = arg1;
                this.arg2 = arg2;
            }

            public BooleanExpression3 getArg1() {
                return arg1;
            }

            public BooleanExpression3 getArg2() {
                return arg2;
            }
        }
    }

    public static class BlnNot implements BlnUnaryOperator {
        @Override
        public Boolean evaluate(BooleanExpression3 arg1) {
            return !arg1.evaluate();
        }

        @Override
        public String toString() {
            return "NOT";
        }


    }

    public static class BlnAnd implements BlnBinaryOperator {
        @Override
        public Boolean evaluate(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            return arg1.evaluate() && arg2.evaluate();
        }

        @Override
        public String toString() {
            return "AND";
        }

        @Override
        public BlnAndBooleanSupplier getBooleanSupplier(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            return new BlnAndBooleanSupplier(arg1, arg2);
        }

        public static class BlnAndBooleanSupplier extends BlnBinaryOpSupplier {

            public BlnAndBooleanSupplier(BooleanExpression3 arg1, BooleanExpression3 arg2) {
                super(arg1, arg2);
            }

            @Override
            public Boolean get() {
                return arg1.evaluate() && arg2.evaluate();
            }

            @Override
            public String toString() {
                return "(" + arg1.getName() + " AND " + arg2.getName() + ")";
            }
        }
    }

    public static class BlnOr implements BlnBinaryOperator {
        @Override
        public Boolean evaluate(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            return arg1.evaluate() || arg2.evaluate();
        }

        @Override
        public String toString() {
            return "OR";
        }

        @Override
        public BlnOrBooleanSupplier getBooleanSupplier(BooleanExpression3 arg1, BooleanExpression3 arg2) {
            return new BlnOrBooleanSupplier(arg1, arg2);
        }

        public static class BlnOrBooleanSupplier extends BlnBinaryOpSupplier {

            public BlnOrBooleanSupplier(BooleanExpression3 arg1, BooleanExpression3 arg2) {
                super(arg1, arg2);
            }

            @Override
            public Boolean get() {
                return arg1.evaluate() || arg2.evaluate();
            }

            @Override
            public String toString() {
                return "(" + arg1.getName() + " OR " + arg2.getName() + ")";
            }
        }
    }
}
