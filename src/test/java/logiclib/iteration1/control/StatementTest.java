package logiclib.iteration1.control;

import org.junit.Test;

import static logiclib.iteration1.control.Statement.IfStatement.*;
import static logiclib.iteration1.control.Statement.SimpleStatement.statement;
import static logiclib.iteration1.expression.BooleanExpression5.bln;

public class StatementTest {

    @Test
    public void testIfStatement() {
//        IfStatement ifStatement1 = ifStatement(bln(true, "A == some value"))
//                .thenStatement((Consumer) (Object f) -> System.out.println("do something"))
//                .elseIfStatement(bln(true, "A == some value"))
//                .thenStatement((Consumer) (Object f) -> System.out.println("do something"));

        Statement statement2 = ifStatement(
                condition(bln(f -> true, "Bank account is empty")),
                thenStatement(statement(f -> System.out.println("do something"), "Alert the customer")),
                elseStatement(
                        ifStatement(
                                condition(bln(true, "Bank account has above transaction amount")),
                                thenStatement(statement(f -> System.out.println("do something"), "Withdraw money")),
                                elseStatement(statement(f -> System.out.println("do something else"), "Cancel the transaction and alert the customer"))
                        )
                )
        );

        // what are we trying to do here....
        // one type of execution is just execution based on facts....
        // but theoretically, a statement could CHANGE the facts, which would CHANGE the value of a child evaluation
        // at runtime.
        // So if something like that happens?
        // How would we actually even be able to utilize the logic tree before execution?
        // Basically, we could only support simple logic.
        // (this is sufficient for rules)
        // and theoretically, a specialized statement type could be used if we were to be CHANGING any of the facts.
        // this would not be easily mapped with a network model, I would guess.
    }
}
