package logiclib.iteration1.expression;

import org.openjdk.jmh.Main;
import org.openjdk.jmh.annotations.*;

import java.util.Random;
import java.util.concurrent.TimeUnit;

import static logiclib.iteration1.expression.BooleanExpression.bln;

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Mode.AverageTime)
public class BooleanExpressionPerformanceTest {

    private Random random = new Random();

    // the (a && b) || (c && d)
    // was
    // BooleanExpressionPerformanceTest.performanceTest1  avgt   20  357.595 � 16.489  ns/op
    // BooleanExpressionPerformanceTest.performanceTest2  avgt   20   40.640 �  0.461  ns/op
    //
    // basically 9 times slower
    // almost an order of magnitude slower.

    // at a slower time:
    //
    // Benchmark                                          Mode  Cnt    Score    Error  Units
    // BooleanExpressionPerformanceTest.performanceTest1  avgt   20  447.793 � 11.851  ns/op
    // BooleanExpressionPerformanceTest.performanceTest2  avgt   20   53.275 �  2.146  ns/op

    // avg. 108-130 ns/op
    @Fork(1)
    @Benchmark
    public boolean performanceTest1() {
        boolean a = random.nextBoolean();
        boolean b = random.nextBoolean();
        boolean c = random.nextBoolean();
        boolean d = random.nextBoolean();
        return bln(() -> a).and(bln(() -> b)).or(
                bln(() -> c).and(bln(() -> d))
        ).evaluate();
    }

    // avg. 25-30 ns/op
    @Fork(1)
    @Benchmark
    public boolean performanceTest2() {
        boolean a = random.nextBoolean();
        boolean b = random.nextBoolean();
        boolean c = random.nextBoolean();
        boolean d = random.nextBoolean();
        return (a && b) || (c && d);
    }

    public static void main(String[] args) throws Exception {
        Main.main(new String[]{BooleanExpressionPerformanceTest.class.getSimpleName()});
    }

}