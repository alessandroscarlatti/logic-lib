package logiclib.iteration1.expression;

import org.openjdk.jmh.Main;
import org.openjdk.jmh.annotations.*;

import java.util.Random;
import java.util.concurrent.TimeUnit;

import static logiclib.iteration1.expression.BooleanExpression3.bln;

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Mode.AverageTime)
public class BooleanExpression3PerformanceTest {


    //so far...10/10/2020
    //Benchmark                                           Mode  Cnt   Score   Error  Units
    //BooleanExpression3PerformanceTest.performanceTest1  avgt   20  72.592 ± 0.870  ns/op
    //BooleanExpression3PerformanceTest.performanceTest2  avgt   20  40.124 ± 0.422  ns/op
    private static BooleanExpression3 expr = bln(
            bln((Facts f) -> f.a, "A")
                    .and((Facts f) -> f.b, "B")
    ).or(
            bln((Facts f) -> f.c, "C")
                    .and((Facts f) -> f.d, "D")
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
        Main.main(new String[]{BooleanExpression3PerformanceTest.class.getSimpleName()});
    }

}