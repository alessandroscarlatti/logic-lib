package logiclib.iteration1;

import java.util.Objects;
import java.util.function.BinaryOperator;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

public class BooleanExpression2 {

    private String name;
    private Boolean value;
    private Throwable exception;  // if exception was encountered during evaluation
    private boolean evaluated;
    private final Supplier<Boolean> supplier;
    private Object userData;

    public BooleanExpression2(Supplier<Boolean> supplier, String name) {
        Objects.requireNonNull(supplier, "Boolean supplier cannot be null");
        this.supplier = supplier;
        this.name = name;
    }

    public BooleanExpression2(Supplier<Boolean> supplier) {
        Objects.requireNonNull(supplier, "Boolean supplier cannot be null");
        this.supplier = supplier;
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

    public BooleanExpression2 and(BooleanExpression2 other) {
        return BlnOperator.AND.apply(this, other);
    }

    public BooleanExpression2 or(BooleanExpression2 other) {
        return BlnOperator.OR.apply(this, other);
    }

    public BooleanExpression2 not() {
        return BlnOperator.NOT.apply(this);
    }

    public static BooleanExpression2 not(BooleanExpression2 other) {
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

    public static BooleanExpression2 bln(Supplier<Boolean> bln) {
        return new BooleanExpression2(bln);
    }

    public static BooleanExpression2 bln(Supplier<Boolean> bln, String name) {
        return new BooleanExpression2(bln, name);
    }

    public static BooleanExpression2 bln(Boolean value) {
        return new BooleanExpression2(new RawBooleanValueSupplier(value));
    }

    public static BooleanExpression2 bln(Boolean value, String name) {
        return new BooleanExpression2(new RawBooleanValueSupplier(value), name);
    }

    public static BooleanExpression2 bln(BooleanExpression2 expr) {
        return new BooleanExpression2(new BooleanExpressionBooleanSupplier(expr), expr.getName());
    }

    public static BooleanExpression2 bln(BooleanExpression2 expr, String name) {
        return new BooleanExpression2(new BooleanExpressionBooleanSupplier(expr), name);
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
        private final BooleanExpression2 expr;

        public BooleanExpressionBooleanSupplier(BooleanExpression2 expr) {
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
    public static BooleanExpression2 bln(Object... args) {
        return new BooleanExpression2(() -> {
            int[] nextIndexWrapper = new int[]{0};

            BooleanExpression2 currentExpr = parseBooleanExpression(args, 0, nextIndexWrapper);

            for (int i = nextIndexWrapper[0] + 1; i < args.length; ) {
                // increment i by 2 each iteration
                try {
                    BlnBinaryOperator op = (BlnBinaryOperator) args[i];
                    BooleanExpression2 rhSide = parseBooleanExpression(args, i + 1, nextIndexWrapper);
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

    private static BooleanExpression2 parseBooleanExpression(Object[] args, int startIndex, int[] endIndex) {
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
        if (args[i] instanceof Boolean || args[i] instanceof BooleanExpression2) {
            // now we are processing the boolean expression
            BooleanExpression2 blnExpr = getBooleanExpression(args[i]);

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

    private static BooleanExpression2 getBooleanExpression(Object arg) {
        if (arg instanceof BooleanExpression2)
            return ((BooleanExpression2) arg);
        else
            return new BooleanExpression2(() -> (Boolean) arg);
    }

    public interface BlnOperator {

        BlnBinaryOperator AND = new BlnAnd();
        BlnBinaryOperator OR = new BlnOr();
        BlnUnaryOperator NOT = new BlnNot();
    }

    public interface BlnUnaryOperator extends BlnOperator, UnaryOperator<BooleanExpression2> {
        Boolean evaluate(BooleanExpression2 arg);

        @Override
        default BooleanExpression2 apply(BooleanExpression2 arg) {
            return new BooleanExpression2(() -> evaluate(arg), toString() + " " + arg.getName());
        }
    }

    public interface BlnBinaryOperator extends BlnOperator, BinaryOperator<BooleanExpression2> {

        Boolean evaluate(BooleanExpression2 arg1, BooleanExpression2 arg2);

        @Override
        default BooleanExpression2 apply(BooleanExpression2 arg1, BooleanExpression2 arg2) {
            return new BooleanExpression2(getBooleanSupplier(arg1, arg2), "(" + arg1.name + " " + toString() + " " + arg2.getName() + ")");
        }

        BlnBinaryOpSupplier getBooleanSupplier(BooleanExpression2 arg1, BooleanExpression2 arg2);

        abstract class BlnBinaryOpSupplier implements Supplier<Boolean> {

            protected BooleanExpression2 arg1;
            protected BooleanExpression2 arg2;

            public BlnBinaryOpSupplier(BooleanExpression2 arg1, BooleanExpression2 arg2) {
                this.arg1 = arg1;
                this.arg2 = arg2;
            }

            public BooleanExpression2 getArg1() {
                return arg1;
            }

            public BooleanExpression2 getArg2() {
                return arg2;
            }
        }
    }

    public static class BlnNot implements BlnUnaryOperator {
        @Override
        public Boolean evaluate(BooleanExpression2 arg1) {
            return !arg1.evaluate();
        }

        @Override
        public String toString() {
            return "NOT";
        }


    }

    public static class BlnAnd implements BlnBinaryOperator {
        @Override
        public Boolean evaluate(BooleanExpression2 arg1, BooleanExpression2 arg2) {
            return arg1.evaluate() && arg2.evaluate();
        }

        @Override
        public String toString() {
            return "AND";
        }

        @Override
        public BlnAndBooleanSupplier getBooleanSupplier(BooleanExpression2 arg1, BooleanExpression2 arg2) {
            return new BlnAndBooleanSupplier(arg1, arg2);
        }

        public static class BlnAndBooleanSupplier extends BlnBinaryOpSupplier {

            public BlnAndBooleanSupplier(BooleanExpression2 arg1, BooleanExpression2 arg2) {
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
        public Boolean evaluate(BooleanExpression2 arg1, BooleanExpression2 arg2) {
            return arg1.evaluate() || arg2.evaluate();
        }

        @Override
        public String toString() {
            return "OR";
        }

        @Override
        public BlnOrBooleanSupplier getBooleanSupplier(BooleanExpression2 arg1, BooleanExpression2 arg2) {
            return new BlnOrBooleanSupplier(arg1, arg2);
        }

        public static class BlnOrBooleanSupplier extends BlnBinaryOpSupplier {

            public BlnOrBooleanSupplier(BooleanExpression2 arg1, BooleanExpression2 arg2) {
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
