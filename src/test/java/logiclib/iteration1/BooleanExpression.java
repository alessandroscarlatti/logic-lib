package logiclib.iteration1;

import java.util.function.BinaryOperator;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

public class BooleanExpression {

    private String name;
    private Boolean value;
    private Throwable exception;  // if exception was encountered during evaluation
    private boolean evaluated;
    private final Supplier<Boolean> supplier;

    public BooleanExpression(Supplier<Boolean> supplier, String name) {
        this.supplier = supplier;
        this.name = name;
    }

    public BooleanExpression(Supplier<Boolean> supplier) {
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
            throw new IllegalStateException("Boolean expression has already been evaluated");

        try {
            value = supplier.get();
            evaluated = true;
            return value;
        } catch (Throwable e) {
            exception = e;
            throw new IllegalStateException("Error evaluating boolean expression", e);
        }
    }

    public static BooleanExpression bln(Boolean value) {
        return new BooleanExpression(() -> value);
    }

    public static BooleanExpression bln(Boolean value, String name) {
        return new BooleanExpression(() -> value, name);
    }

    public static BooleanExpression bln(BooleanExpression expr) {
        return new BooleanExpression(expr::evaluate, expr.getName());
    }

    public static BooleanExpression bln(BooleanExpression expr, String name) {
        return new BooleanExpression(expr::evaluate, name);
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
            return new BooleanExpression(() -> evaluate(arg));
        }
    }

    public interface BlnBinaryOperator extends BlnOperator, BinaryOperator<BooleanExpression> {

        Boolean evaluate(BooleanExpression arg1, BooleanExpression arg2);

        @Override
        default BooleanExpression apply(BooleanExpression arg1, BooleanExpression arg2) {
            return new BooleanExpression(() -> evaluate(arg1, arg2), "(" + arg1.name + " " + toString() + " " + arg2.getName() + ")");
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
    }
}
