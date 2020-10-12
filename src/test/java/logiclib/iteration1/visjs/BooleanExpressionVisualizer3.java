package logiclib.iteration1.visjs;

import com.fasterxml.jackson.databind.ObjectMapper;
import logiclib.iteration1.BooleanExpression5;
import logiclib.iteration1.BooleanExpression5.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static logiclib.iteration1.BooleanExpression5.*;

public class BooleanExpressionVisualizer3 {

    public static void main(String[] args) throws Exception {
        BooleanExpression5 expr1 = or(
                and(bln(f -> true, "A"),
                        bln(f -> false, "B")
                ),
                or(
                        bln(f -> true, "C"),
                        bln(f -> false, "D")
                ),
                and(
                        bln(false, "E"),
                        bln(false, "F"),
                        bln(false, "G")
                ),
                and(
                        bln(false, "H"),
                        bln(false, "I"),
                        bln(true, "J"),
                        bln(false, "K"),
                        bln(true, "L")
                )
        );

        class Facts {
            int a = 0;
            int b = 1;
            int c = 2;

            public Facts(int a, int b, int c) {
                this.a = a;
                this.b = b;
                this.c = c;
            }
        }

        BooleanExpression5 expr2 = or(
                bln(false, "A"),
                bln(false, "B"),
                bln(false, "C"),
                bln(false, "D"),
                and(
                        bln(true, "E"),
                        bln(false, "F"),
                        bln(false, "G")
                ),
                and(
                        bln(true, "H"),
                        bln(true, "I")
                ),
                and(
                        bln(false, "J"),
                        bln(true, "K")
                )
        );

        BooleanExpression5 expr3 = and(
                bln((Facts f) -> f.a == 0, "a == 0"),
                bln((Facts f) -> f.b == 1, "b == 1"),
                bln((Facts f) -> f.c == 2, "c == 2")
        );

        expr1.evaluate(new Facts(-1234, 0, 3));
        expr2.evaluate(new Facts(-1234, 0, 3));
        expr3.evaluate(new Facts(-1234, 0, 3));

        Network network = getNetwork(Arrays.asList(expr1, expr2, expr3));
//        Path file = Paths.get("sandbox/network.json");
        Path file = Paths.get("C:\\workspace\\projects\\visjs-ui1\\src\\main\\resources\\static\\examples\\network\\network.json");
        Files.createDirectories(file.getParent());
        Files.write(file, new ObjectMapper().writerWithDefaultPrettyPrinter().writeValueAsBytes(network));
    }

    private static Network getNetwork(List<BooleanExpression5> expressions) {
        Network network = new Network();
        for (BooleanExpression5 expression : expressions) {
            populateNetwork(network, expression);
        }
        return network;
    }

    private static void populateNetwork(Network network, BooleanExpression5 expr) {

        BooleanExpressionVisitor visitor = new BooleanExpressionVisitor() {

            private boolean foundFirstBinaryOperator = false;
            //            private final Map<String, BooleanExpression5> allExpressionParents = new HashMap<>();
//            private final Map<String, BooleanExpression5> orExpressionParents = new HashMap<>();
//            private final Map<String, BooleanExpression5> andExpressionParents = new HashMap<>();
            private final String uuid = UUID.randomUUID().toString();

            @Override
            public void visitBooleanAnd(BooleanAnd booleanAnd) {
                System.out.println("booleanAnd = " + booleanAnd);
                visitFirstExpression(booleanAnd);

                Network.Node nodeAnd = new Network.Node();
                nodeAnd.setId(uuid + "_" + booleanAnd.getName());
                nodeAnd.setLabel("AND");
                nodeAnd.setColor("orange");
                nodeAnd.setShape("circle");
                network.getNodes().add(nodeAnd);

                for (BooleanExpression5 arg : booleanAnd.getArgs()) {
                    Network.Edge edge = new Network.Edge();
                    edge.setFrom(uuid + "_" + arg.getName());
                    edge.setTo(uuid + "_" + booleanAnd.getName());
                    // edge.setArrows("to");

                    if (arg.isEvaluated()) {
                        edge.setColor(arg.getValue() ? "lightgreen" : "orangered");
                        edge.getFont().setColor(arg.getValue() ? "lightgreen" : "orangered");
                        edge.getFont().setStrokeColor("#343434");
                        edge.setLabel(String.valueOf(arg.getValue()));
                    } else {
                        edge.setColor("lightblue");
                    }

                    network.getEdges().add(edge);
                }
            }

            @Override
            public void visitBooleanOr(BooleanOr booleanOr) {
                System.out.println("booleanOr = " + booleanOr);
                visitFirstExpression(booleanOr);

                Network.Node nodeAnd = new Network.Node();
                nodeAnd.setId(uuid + "_" + booleanOr.getName());
                nodeAnd.setLabel("OR");
                nodeAnd.setColor("orange");
                nodeAnd.setShape("circle");
                network.getNodes().add(nodeAnd);

                for (BooleanExpression5 arg : booleanOr.getArgs()) {
                    Network.Edge edge = new Network.Edge();
                    edge.setFrom(uuid + "_" + arg.getName());
                    edge.setTo(uuid + "_" + booleanOr.getName());
                    // edge.setArrows("to");

                    if (arg.isEvaluated()) {
                        edge.setColor(arg.getValue() ? "lightgreen" : "orangered");
                        edge.getFont().setColor(arg.getValue() ? "lightgreen" : "orangered");
                        edge.getFont().setStrokeColor("#343434");
                        edge.setLabel(String.valueOf(arg.getValue()));
                    } else {
                        edge.setColor("lightblue");
                    }

                    network.getEdges().add(edge);
                }
            }

            @Override
            public void visitBooleanNot(BooleanNot booleanNot) {
                System.out.println("booleanNot = " + booleanNot);
                visitFirstExpression(booleanNot);
            }

            @Override
            public void visitBooleanValue(BooleanValue booleanValue) {
                System.out.println("booleanValue = " + booleanValue);
                visitFirstExpression(booleanValue);
                visitLeafExpression(booleanValue);
            }

            @Override
            public void visitBooleanFunction(BooleanFunction booleanFunction) {
                System.out.println("booleanFunction = " + booleanFunction);
                visitFirstExpression(booleanFunction);
                visitLeafExpression(booleanFunction);
            }

            private void visitFirstExpression(BooleanExpression5 opExpression) {
                if (!foundFirstBinaryOperator) {
                    // only run this functionality once

                    foundFirstBinaryOperator = true;
                    // this is the first binary operator, so create a result node that connects to this
                    Network.Node node1 = new Network.Node();
                    node1.setId(uuid + "_" + opExpression.getName() + "_start");
                    node1.setLabel(opExpression.isEvaluated() ? String.valueOf(opExpression.getValue()) : "Result");
                    node1.setColor(opExpression.isEvaluated() ? (opExpression.getValue() ? "lightgreen" : "orangered") : "lightblue");
                    node1.setShape("circle");

                    Network.Edge edge1 = new Network.Edge();
                    edge1.setFrom(uuid + "_" + opExpression.getName());
                    edge1.setTo(uuid + "_" + opExpression.getName() + "_start");
                    // edge1.setArrows("to");

                    if (opExpression.isEvaluated()) {
                        edge1.setColor(opExpression.getValue() ? "lightgreen" : "orangered");
                        edge1.getFont().setColor(opExpression.getValue() ? "lightgreen" : "orangered");
                        edge1.getFont().setStrokeColor("#343434");
                        edge1.setLabel(String.valueOf(opExpression.getValue()));
                    } else {
                        edge1.setColor("lightblue");
                    }

                    network.getNodes().add(node1);
                    network.getEdges().add(edge1);
                }
            }

            private void visitLeafExpression(BooleanExpression5 leafExpression) {
                Network.Node node1 = new Network.Node();
                node1.setId(uuid + "_" + leafExpression.getName());
                node1.setLabel(leafExpression.getName());
                node1.setShape("box");
                node1.setColor("lightblue");

                network.getNodes().add(node1);
            }
        };

        expr.visit(visitor);
    }
}
