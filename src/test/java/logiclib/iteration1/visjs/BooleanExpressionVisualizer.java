package logiclib.iteration1.visjs;

import com.fasterxml.jackson.databind.ObjectMapper;
import logiclib.iteration1.BooleanExpression3;
import logiclib.iteration1.BooleanExpression3.BlnExpressionVisitor;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static logiclib.iteration1.BooleanExpression3.bln;

public class BooleanExpressionVisualizer {

    public static void main(String[] args) throws Exception {
//        BooleanExpression3 expr = bln(
//                bln(f -> true, "A")
//                        .and(f -> false, "B")
//        ).or(
//                bln(f -> true, "C")
//                        .and(f -> false, "D")
//        ).or(
//                bln(false, "E")
//                        .and(false, "F")
//                        .and(false, "G")
//        ).or(
//                bln(false, "H")
//                        .or(false, "I")
//                        .or(false, "J")
//                        .or(false, "K")
//                        .or(false, "L")
//        );
        BooleanExpression3 expr = bln(true, "A")
                .and(true, "B")
                .and(true, "C")
                .and(false, "D")
                .or(false, "E")
                .or(false, "F")
                .or(false, "G")
                .and(false, "H")
                .and(false, "I")
                .or(
                        bln(false, "J")
                        .and(true, "K")
                );

        Network network = getNetwork(expr);
//        Path file = Paths.get("sandbox/network.json");
        Path file = Paths.get("C:\\workspace\\projects\\visjs-ui1\\src\\main\\resources\\static\\examples\\network\\network.json");
        Files.createDirectories(file.getParent());
        Files.write(file, new ObjectMapper().writerWithDefaultPrettyPrinter().writeValueAsBytes(network));
    }

    private static Network getNetwork(BooleanExpression3 expr) {
        Network network = new Network();

        BlnExpressionVisitor visitor = new BlnExpressionVisitor() {

            private boolean foundFirstBinaryOperator = false;
//            private final Map<String, BooleanExpression3> allExpressionParents = new HashMap<>();
            private final Map<String, BooleanExpression3> orExpressionParents = new HashMap<>();
            private final Map<String, BooleanExpression3> andExpressionParents = new HashMap<>();

            @Override
            public void visitBooleanAnd(BooleanExpression3 andExpression, BooleanExpression3 arg1, BooleanExpression3 arg2) {
                if (!foundFirstBinaryOperator) {
                    // this is the first binary operator, so create a result node that connects to this
                    foundFirstBinaryOperator = true;
                    Network.Node node1 = new Network.Node();
                    node1.setId(andExpression.getName() + "_start");
                    node1.setLabel("Result");
                    node1.setColor("lightgreen");
                    node1.setShape("circle");

                    Network.Edge edge1 = new Network.Edge();
                    edge1.setFrom(andExpression.getName());
                    edge1.setTo(andExpression.getName() + "_start");
                    edge1.setColor("lightgray");

                    network.getNodes().add(node1);
                    network.getEdges().add(edge1);
                }

                System.out.println("AND arg1 = " + arg1 + ", arg2 = " + arg2);
                BooleanExpression3 andExpressionToUse = andExpression;  // todo calculate this


                if (!andExpressionParents.containsKey(andExpression.getName())) {
                    Network.Node node1 = new Network.Node();
                    node1.setId(andExpression.getName());
                    node1.setLabel("AND");
                    node1.setColor("orange");
                    node1.setShape("circle");
                    network.getNodes().add(node1);
                    andExpressionParents.put(arg1.getName(), andExpression);  // make it so that when arg1 comes up, it can find this parent
                    andExpressionParents.put(arg2.getName(), andExpression);  // make it so that when arg2 comes up, it can find this parent
                } else {
                    andExpressionToUse = andExpressionParents.get(andExpression.getName());
                    andExpressionParents.put(arg1.getName(), andExpressionToUse);  // make it so that when arg1 comes up, it can find this parent
                    andExpressionParents.put(arg2.getName(), andExpressionToUse);  // make it so that when arg2 comes up, it can find this parent
                }

                Network.Edge edge1 = new Network.Edge();
                edge1.setFrom(arg1.getName());
                edge1.setTo(andExpressionToUse.getName());
                edge1.setColor("lightgray");

                Network.Edge edge2 = new Network.Edge();
                edge2.setFrom(arg2.getName());
                edge2.setTo(andExpressionToUse.getName());
                edge2.setColor("lightgray");

                network.getEdges().add(edge1);
                network.getEdges().add(edge2);
            }

            @Override
            public void visitBooleanOr(BooleanExpression3 orExpression, BooleanExpression3 arg1, BooleanExpression3 arg2) {
                System.out.println("OR arg1 = " + arg1 + ", arg2 = " + arg2);

                if (!foundFirstBinaryOperator) {
                    // this is the first binary operator, so create a result node that connects to this
                    foundFirstBinaryOperator = true;
                    Network.Node node1 = new Network.Node();
                    node1.setId(orExpression.getName() + "_start");
                    node1.setLabel("Result");
                    node1.setColor("lightgreen");
                    node1.setShape("circle");

                    Network.Edge edge1 = new Network.Edge();
                    edge1.setFrom(orExpression.getName());
                    edge1.setTo(orExpression.getName() + "_start");
                    edge1.setColor("lightgray");

                    network.getNodes().add(node1);
                    network.getEdges().add(edge1);
                }

                // HOW DO WE KNOW that this node is an OR connected to a previous OR?
                // if so, we can collapse this OR and use the previous one instead

                // possibly create a new OR node

                // ask if the arg1 is another or...
                System.out.println("OR arg1 = " + arg1 + ", arg2 = " + arg2);

                BooleanExpression3 orExpressionToUse = orExpression;  // todo calculate this


                if (!orExpressionParents.containsKey(orExpression.getName())) {
                    Network.Node node1 = new Network.Node();
                    node1.setId(orExpression.getName());
                    node1.setLabel("OR");
                    node1.setColor("orange");
                    node1.setShape("circle");
                    network.getNodes().add(node1);
                    orExpressionParents.put(arg1.getName(), orExpression);  // make it so that when arg1 comes up, it can find this parent
                    orExpressionParents.put(arg2.getName(), orExpression);  // make it so that when arg2 comes up, it can find this parent
                } else {
                    orExpressionToUse = orExpressionParents.get(orExpression.getName());
                    orExpressionParents.put(arg1.getName(), orExpressionToUse);  // make it so that when arg1 comes up, it can find this parent
                    orExpressionParents.put(arg2.getName(), orExpressionToUse);  // make it so that when arg2 comes up, it can find this parent
                }

                Network.Edge edge1 = new Network.Edge();
                edge1.setFrom(arg1.getName());
                edge1.setTo(orExpressionToUse.getName());
                edge1.setColor("lightgray");

                Network.Edge edge2 = new Network.Edge();
                edge2.setFrom(arg2.getName());
                edge2.setTo(orExpressionToUse.getName());
                edge2.setColor("lightgray");

                network.getEdges().add(edge1);
                network.getEdges().add(edge2);
            }

            @Override
            public void visitBooleanLeaf(BooleanExpression3 leaf) {
                System.out.println("Leaf " + leaf);

                Network.Node node1 = new Network.Node();
                node1.setId(leaf.getName());
                node1.setLabel(leaf.getName());
                node1.setColor("lightblue");
                node1.setShape("box");

                network.getNodes().add(node1);
            }

            @Override
            public void visitBooleanOr(BooleanExpression3 arg1, BooleanExpression3 arg2) {
                // need to be able to tell if the expression is a leaf

                // when we move up by one level...reusing the previous AND expression as the argument,
                // we don't want to duplicate the AND

                // A, A AND B, B that's the first item to be sent to a callback visitBooleanAnd
                // the visitor would then move to the each child...the same way....
                // we will only visitBooleanLeaf
            }
        };

        expr.visit(visitor);

        return network;
    }
}
