package logiclib.iteration1.expression;

import org.openjdk.jmh.Main;
import org.openjdk.jmh.annotations.*;

import java.util.Random;
import java.util.concurrent.TimeUnit;

import static logiclib.iteration1.expression.BooleanExpression5.*;

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Mode.AverageTime)
public class BooleanExpression5PerformanceTest {


    //Benchmark                                           Mode  Cnt   Score   Error  Units
    //BooleanExpression5PerformanceTest.performanceTest1  avgt   20  75.601 ± 2.405  ns/op
    //BooleanExpression5PerformanceTest.performanceTest2  avgt   20  41.733 ± 2.437  ns/op

    private static BooleanExpression5 expr = or(
            and(
                    bln((Facts f) -> f.a, "A"),
                    bln((Facts f) -> f.b, "B")
            ),
            and(
                    bln((Facts f) -> f.c, "C"),
                    bln((Facts f) -> f.d, "D")
            )
    );

    // avg. 108-130 ns/op
    @Fork(1)
    @Benchmark
    public boolean performanceTest1() {
        Facts facts = new Facts();
        return expr.evaluate(facts);
    }

    static class Facts {

        private static Random random = new Random();

        boolean a = random.nextBoolean();
        boolean b = random.nextBoolean();
        boolean c = random.nextBoolean();
        boolean d = random.nextBoolean();
    }

    // avg. 25-30 ns/op
    @Fork(1)
    @Benchmark
    public boolean performanceTest2() {
        Facts facts = new Facts();

        return (facts.a && facts.b) || (facts.c && facts.d);
    }

    public static void main(String[] args) throws Exception {
        Main.main(new String[]{BooleanExpression5PerformanceTest.class.getSimpleName()});
    }

}