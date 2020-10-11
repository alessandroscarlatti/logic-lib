package logiclib.iteration1.visjs;

import com.fasterxml.jackson.databind.ObjectMapper;
import logiclib.iteration1.BooleanExpression4;
import logiclib.iteration1.BooleanExpression4.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static logiclib.iteration1.BooleanExpression4.bln;

public class BooleanExpressionVisualizer2 {

    public static void main(String[] args) throws Exception {
        BooleanExpression4 expr1 = bln(
                bln(f -> true, "AAAA")
                        .and(f -> false, "B")
        ).or(
                bln(f -> true, "C")
                        .and(f -> false, "D")
        ).or(
                bln(false, "E")
                        .and(false, "F")
                        .and(false, "G")
        ).or(
                bln(false, "H")
                        .or(false, "I")
                        .or(true, "J")
                        .or(false, "K")
                        .or(true, "L")
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

        BooleanExpression4 expr2 = bln(true, "A")
                .and(true, "B")
                .and(true, "C")
                .and(false, "D")
                .and(
                        bln(false, "E")
                                .or(false, "F")
                                .or(false, "G")
                )
                .and(false, "H")
                .and(false, "I")
                .or(
                        bln(false, "J")
                                .and(true, "K")
                );

        BooleanExpression4 expr3 = bln((Facts f) -> f.a == 0, "a == 0")
                .or((Facts f) -> f.b == 1, "b == 1")
                .or((Facts f) -> f.c == 2, "c == 2");

        expr1.evaluate(new Facts(-1234, 0, 3));
        expr2.evaluate(new Facts(-1234, 0, 3));
//        expr3.evaluate(new Facts(-1234, 0, 3));

        Network network = getNetwork(Arrays.asList(expr1, expr2, expr3));
//        Path file = Paths.get("sandbox/network.json");
        Path file = Paths.get("C:\\workspace\\projects\\visjs-ui1\\src\\main\\resources\\static\\examples\\network\\network.json");
        Files.createDirectories(file.getParent());
        Files.write(file, new ObjectMapper().writerWithDefaultPrettyPrinter().writeValueAsBytes(network));
    }

    private static Network getNetwork(List<BooleanExpression4> expressions) {
        Network network = new Network();
        for (BooleanExpression4 expression : expressions) {
            populateNetwork(network, expression);
        }
        return network;
    }

    private static void populateNetwork(Network network, BooleanExpression4 expr) {

        BooleanExpressionVisitor visitor = new BooleanExpressionVisitor() {

            private boolean foundFirstBinaryOperator = false;
            //            private final Map<String, BooleanExpression4> allExpressionParents = new HashMap<>();
            private final Map<String, BooleanExpression4> orExpressionParents = new HashMap<>();
            private final Map<String, BooleanExpression4> andExpressionParents = new HashMap<>();
            private final String uuid = UUID.randomUUID().toString();

            @Override
            public void visitBooleanAnd(BooleanAnd booleanAnd) {
                System.out.println("booleanAnd = " + booleanAnd);
                visitFirstExpression(booleanAnd);

                BooleanExpression4 arg1 = booleanAnd.getArg1();
                BooleanExpression4 arg2 = booleanAnd.getArg2();

                BooleanExpression4 andExpressionToUse = booleanAnd;

                if (!andExpressionParents.containsKey(booleanAnd.getName())) {
                    // this is the first time we've encountered this "AND node"
                    // it has NOT been encountered yet as the PARENT of any other node
                    // (a AND b) AND C
                    Network.Node node1 = new Network.Node();
                    node1.setId(uuid + "_" + booleanAnd.getName());
                    node1.setLabel("AND");
                    node1.setColor("orange");
                    node1.setShape("circle");
                    network.getNodes().add(node1);
                    andExpressionParents.put(arg1.getName(), booleanAnd);  // make it so that when arg1 comes up, it can find this parent
                    andExpressionParents.put(arg2.getName(), booleanAnd);  // make it so that when arg2 comes up, it can find this parent
                } else {
                    // we've already encountered this node's condensed AND parent
                    andExpressionToUse = andExpressionParents.get(booleanAnd.getName());
                    andExpressionParents.put(arg1.getName(), andExpressionToUse);  // make it so that when arg1 comes up, it can find this parent
                    andExpressionParents.put(arg2.getName(), andExpressionToUse);  // make it so that when arg2 comes up, it can find this parent
                }

                Network.Edge edge1 = new Network.Edge();
                edge1.setFrom(uuid + "_" + arg1.getName());
                edge1.setTo(uuid + "_" + andExpressionToUse.getName());
//                edge1.setArrows("to");

                if (arg1.isEvaluated()) {
                    edge1.setColor(arg1.getValue() ? "lightgreen" : "orangered");
                    edge1.getFont().setColor(arg1.getValue() ? "lightgreen" : "orangered");
                    edge1.getFont().setStrokeColor("#343434");
                    edge1.setLabel(String.valueOf(arg1.getValue()));
                } else {
                    edge1.setColor("lightblue");
                }

                Network.Edge edge2 = new Network.Edge();
                edge2.setFrom(uuid + "_" + arg2.getName());
                edge2.setTo(uuid + "_" + andExpressionToUse.getName());
//                edge2.setArrows("to");
                if (arg2.isEvaluated()) {
                    edge2.setColor(arg2.getValue() ? "lightgreen" : "orangered");
                    edge2.getFont().setColor(arg2.getValue() ? "lightgreen" : "orangered");
                    edge2.getFont().setStrokeColor("#343434");
                    edge2.setLabel(String.valueOf(arg2.getValue()));
                } else {
                    edge2.setColor("lightblue");
                }

                network.getEdges().add(edge1);
                network.getEdges().add(edge2);
            }

            @Override
            public void visitBooleanOr(BooleanOr booleanOr) {
                System.out.println("booleanOr = " + booleanOr);
                visitFirstExpression(booleanOr);

                BooleanExpression4 orExpressionToUse = booleanOr;
                BooleanExpression4 arg1 = booleanOr.getArg1();
                BooleanExpression4 arg2 = booleanOr.getArg2();

                if (!orExpressionParents.containsKey(booleanOr.getName())) {
                    Network.Node node1 = new Network.Node();
                    node1.setId(uuid + "_" + booleanOr.getName());
                    node1.setLabel("OR");
                    node1.setColor("orange");
                    node1.setShape("circle");
                    network.getNodes().add(node1);
                    orExpressionParents.put(arg1.getName(), booleanOr);  // make it so that when arg1 comes up, it can find this parent
                    orExpressionParents.put(arg2.getName(), booleanOr);  // make it so that when arg2 comes up, it can find this parent
                } else {
                    orExpressionToUse = orExpressionParents.get(booleanOr.getName());
                    orExpressionParents.put(arg1.getName(), orExpressionToUse);  // make it so that when arg1 comes up, it can find this parent
                    orExpressionParents.put(arg2.getName(), orExpressionToUse);  // make it so that when arg2 comes up, it can find this parent
                }

                Network.Edge edge1 = new Network.Edge();
                edge1.setFrom(uuid + "_" + arg1.getName());
                edge1.setTo(uuid + "_" + orExpressionToUse.getName());
//                edge1.setArrows("to");

                if (arg1.isEvaluated()) {
                    edge1.setColor(arg1.getValue() ? "lightgreen" : "orangered");
                    edge1.getFont().setColor(arg1.getValue() ? "lightgreen" : "orangered");
                    edge1.getFont().setStrokeColor("#343434");
                    edge1.setLabel(String.valueOf(arg1.getValue()));
                } else {
                    edge1.setColor("lightblue");
                }

                Network.Edge edge2 = new Network.Edge();
                edge2.setFrom(uuid + "_" + arg2.getName());
                edge2.setTo(uuid + "_" + orExpressionToUse.getName());
//                edge2.setArrows("to");

                if (arg2.isEvaluated()) {
                    edge2.setColor(arg2.getValue() ? "lightgreen" : "orangered");
                    edge2.getFont().setColor(arg2.getValue() ? "lightgreen" : "orangered");
                    edge2.getFont().setStrokeColor("#343434");
                    edge2.setLabel(String.valueOf(arg2.getValue()));
                } else {
                    edge2.setColor("lightblue");
                }

                network.getEdges().add(edge1);
                network.getEdges().add(edge2);
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

            private void visitFirstExpression(BooleanExpression4 opExpression) {
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

            private void visitLeafExpression(BooleanExpression4 leafExpression) {
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
