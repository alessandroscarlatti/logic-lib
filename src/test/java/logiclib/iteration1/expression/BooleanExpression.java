package logiclib.iteration1.expression;

import java.util.Objects;
import java.util.function.BinaryOperator;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

public class BooleanExpression {

    private String name;
    private Boolean value;
    private Throwable exception;  // if exception was encountered during evaluation
    private boolean evaluated;
    private final Supplier<Boolean> supplier;
    private Object userData;

    public BooleanExpression(Supplier<Boolean> supplier, String name) {
        Objects.requireNonNull(supplier, "Boolean supplier cannot be null");
        this.supplier = supplier;
        this.name = name;
    }

    public BooleanExpression(Supplier<Boolean> supplier) {
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

    public BooleanExpression and(BooleanExpression other) {
        return BlnOperator.AND.apply(this, other);
    }

    public BooleanExpression or(BooleanExpression other) {
        return BlnOperator.OR.apply(this, other);
    }

    public BooleanExpression not() {
        return BlnOperator.NOT.apply(this);
    }

    public static BooleanExpression not(BooleanExpression other) {
        return BlnOperator.NOT.apply(other);
    }

    public Boolean evaluate() {
        if (isEvaluated())
            return value;
            // throw new IllegalStateException("Boolean expression has already been evaluated");

        try {
            value = supplier.get();
            evaluated = true;
            return value;
        } catch (Throwable e) {
            exception = e;
            throw new IllegalStateException("Error evaluating boolean expression", e);
        }
    }

    public static BooleanExpression bln(Supplier<Boolean> bln) {
        return new BooleanExpression(bln);
    }

    public static BooleanExpression bln(Supplier<Boolean> bln, String name) {
        return new BooleanExpression(bln, name);
    }

    public static BooleanExpression bln(Boolean value) {
        return new BooleanExpression(new RawBooleanValueSupplier(value));
    }

    public static BooleanExpression bln(Boolean value, String name) {
        return new BooleanExpression(new RawBooleanValueSupplier(value), name);
    }

    public static BooleanExpression bln(BooleanExpression expr) {
        return new BooleanExpression(new BooleanExpressionBooleanSupplier(expr), expr.getName());
    }

    public static BooleanExpression bln(BooleanExpression expr, String name) {
        return new BooleanExpression(new BooleanExpressionBooleanSupplier(expr), name);
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
        private final BooleanExpression expr;

        public BooleanExpressionBooleanSupplier(BooleanExpression expr) {
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
    public static BooleanExpression bln(Object... args) {
        return new BooleanExpression(() -> {
            int[] nextIndexWrapper = new int[]{0};

            BooleanExpression currentExpr = parseBooleanExpression(args, 0, nextIndexWrapper);

            for (int i = nextIndexWrapper[0] + 1; i < args.length; ) {
                // increment i by 2 each iteration
                try {
                    BlnBinaryOperator op = (BlnBinaryOperator) args[i];
                    BooleanExpression rhSide = parseBooleanExpression(args, i + 1, nextIndexWrapper);
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

    private static BooleanExpression parseBooleanExpression(Object[] args, int startIndex, int[] endIndex) {
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
        if (args[i] instanceof Boolean || args[i] instanceof BooleanExpression) {
            // now we are processing the boolean expression
            BooleanExpression blnExpr = getBooleanExpression(args[i]);

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

    private static BooleanExpression getBooleanExpression(Object arg) {
        if (arg instanceof BooleanExpression)
            return ((BooleanExpression) arg);
        else
            return new BooleanExpression(() -> (Boolean) arg);
    }

    public interface BlnOperator {

        BlnBinaryOperator AND = new BlnAnd();
        BlnBinaryOperator OR = new BlnOr();
        BlnUnaryOperator NOT = new BlnNot();
    }

    public interface BlnUnaryOperator extends BlnOperator, UnaryOperator<BooleanExpression> {
        Boolean evaluate(BooleanExpression arg);

        @Override
        default BooleanExpression apply(BooleanExpression arg) {
            return new BooleanExpression(() -> evaluate(arg), toString() + " " + arg.getName());
        }
    }

    public interface BlnBinaryOperator extends BlnOperator, BinaryOperator<BooleanExpression> {

        Boolean evaluate(BooleanExpression arg1, BooleanExpression arg2);

        @Override
        default BooleanExpression apply(BooleanExpression arg1, BooleanExpression arg2) {
            return new BooleanExpression(getBooleanSupplier(arg1, arg2), "(" + arg1.name + " " + toString() + " " + arg2.getName() + ")");
        }

        BlnBinaryOpSupplier getBooleanSupplier(BooleanExpression arg1, BooleanExpression arg2);

        abstract class BlnBinaryOpSupplier implements Supplier<Boolean> {

            protected BooleanExpression arg1;
            protected BooleanExpression arg2;

            public BlnBinaryOpSupplier(BooleanExpression arg1, BooleanExpression arg2) {
                this.arg1 = arg1;
                this.arg2 = arg2;
            }

            public BooleanExpression getArg1() {
                return arg1;
            }

            public BooleanExpression getArg2() {
                return arg2;
            }
        }
    }

    public static class BlnNot implements BlnUnaryOperator {
        @Override
        public Boolean evaluate(BooleanExpression arg1) {
            return !arg1.evaluate();
        }

        @Override
        public String toString() {
            return "NOT";
        }


    }

    public static class BlnAnd implements BlnBinaryOperator {
        @Override
        public Boolean evaluate(BooleanExpression arg1, BooleanExpression arg2) {
            return arg1.evaluate() && arg2.evaluate();
        }

        @Override
        public String toString() {
            return "AND";
        }

        @Override
        public BlnAndBooleanSupplier getBooleanSupplier(BooleanExpression arg1, BooleanExpression arg2) {
            return new BlnAndBooleanSupplier(arg1, arg2);
        }

        public static class BlnAndBooleanSupplier extends BlnBinaryOpSupplier {

            public BlnAndBooleanSupplier(BooleanExpression arg1, BooleanExpression arg2) {
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
        public Boolean evaluate(BooleanExpression arg1, BooleanExpression arg2) {
            return arg1.evaluate() || arg2.evaluate();
        }

        @Override
        public String toString() {
            return "OR";
        }

        @Override
        public BlnOr.BlnOrBooleanSupplier getBooleanSupplier(BooleanExpression arg1, BooleanExpression arg2) {
            return new BlnOrBooleanSupplier(arg1, arg2);
        }

        public static class BlnOrBooleanSupplier extends BlnBinaryOpSupplier {

            public BlnOrBooleanSupplier(BooleanExpression arg1, BooleanExpression arg2) {
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
