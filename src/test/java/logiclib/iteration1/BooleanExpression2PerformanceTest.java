package logiclib.iteration1;

import org.openjdk.jmh.Main;
import org.openjdk.jmh.annotations.*;

import java.util.Random;
import java.util.concurrent.TimeUnit;

import static logiclib.iteration1.BooleanExpression2.bln;

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Mode.AverageTime)
public class BooleanExpression2PerformanceTest {

    private Random random = new Random();
    private BlnSupplier cachingBooleanExpression = new BlnSupplier();

    // computer was running slower....with the (A && B) || (C && D)
    //
    // Benchmark                                           Mode  Cnt   Score   Error  Units
    // BooleanExpression2PerformanceTest.performanceTest1  avgt   20  91.347 � 0.971  ns/op
    // BooleanExpression2PerformanceTest.performanceTest2  avgt   20  52.633 � 1.708  ns/op

    // (a && b) || (c && d) || (e && f) || (g && h);
    // Benchmark                                           Mode  Cnt    Score    Error  Units
    // BooleanExpression2PerformanceTest.performanceTest1  avgt   20  212.713 � 20.093  ns/op
    // BooleanExpression2PerformanceTest.performanceTest2  avgt   20   98.160 �  2.805  ns/op

    private static class BlnSupplier {
        private boolean a;
        private boolean b;
        private boolean c;
        private boolean d;
        private boolean e;
        private boolean f;
        private boolean g;
        private boolean h;

        private final BooleanExpression2 expr;

        public BlnSupplier() {
            expr = bln(() -> a, "A").and(bln(() -> b, "B")).or(
                    bln(() -> c, "C").and(bln(() -> d, "D"))
            ).or(
                    bln(() -> e, "C").and(bln(() -> f, "D"))
            ).or(
                    bln(() -> g, "C").and(bln(() -> h, "D"))
            );
        }

        public void setFacts(boolean a, boolean b, boolean c, boolean d, boolean e, boolean f, boolean g, boolean h) {
            this.a = a;
            this.b = b;
            this.c = c;
            this.d = d;
            this.e = e;
            this.f = f;
            this.g = g;
            this.h = h;
        }

        private boolean evaluate() {
            return expr.evaluate();
        }
    }

    // the (a && b) || (c && d)
    // was
    // BooleanExpressionPerformanceTest.performanceTest1  avgt   20  357.595 � 16.489  ns/op
    // BooleanExpressionPerformanceTest.performanceTest2  avgt   20   40.640 �  0.461  ns/op
    //
    // basically 9 times slower
    // almost an order of magnitude slower.

    // avg. 108-130 ns/op
    @Fork(1)
    @Benchmark
    public boolean performanceTest1() {
        boolean a = random.nextBoolean();
        boolean b = random.nextBoolean();
        boolean c = random.nextBoolean();
        boolean d = random.nextBoolean();
        boolean e = random.nextBoolean();
        boolean f = random.nextBoolean();
        boolean g = random.nextBoolean();
        boolean h = random.nextBoolean();
        cachingBooleanExpression.setFacts(a, b, c, d, e, f, g, h);
        return cachingBooleanExpression.evaluate();
    }

    // avg. 25-30 ns/op
    @Fork(1)
    @Benchmark
    public boolean performanceTest2() {
        boolean a = random.nextBoolean();
        boolean b = random.nextBoolean();
        boolean c = random.nextBoolean();
        boolean d = random.nextBoolean();
        boolean e = random.nextBoolean();
        boolean f = random.nextBoolean();
        boolean g = random.nextBoolean();
        boolean h = random.nextBoolean();
        return (a && b) || (c && d) || (e && f) || (g && h);
    }

    public static void main(String[] args) throws Exception {
        Main.main(new String[]{BooleanExpression2PerformanceTest.class.getSimpleName()});
    }

}