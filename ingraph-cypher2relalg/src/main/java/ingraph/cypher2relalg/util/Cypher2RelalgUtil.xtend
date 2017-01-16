package ingraph.cypher2relalg.util

import java.util.Iterator
import java.util.LinkedList
import java.util.List
import java.util.Set
import relalg.ArithmeticOperationExpression
import relalg.BinaryLogicalOperatorType
import relalg.BinaryOperator
import relalg.EmptyListExpression
import relalg.ExpandOperator
import relalg.Expression
import relalg.ExpressionVariable
import relalg.FunctionExpression
import relalg.GetVerticesOperator
import relalg.GraphObjectVariable
import relalg.JoinOperator
import relalg.LeftOuterJoinOperator
import relalg.ListExpression
import relalg.Literal
import relalg.LogicalExpression
import relalg.Operator
import relalg.RelalgContainer
import relalg.RelalgFactory
import relalg.UnaryArithmeticOperationExpression
import relalg.UnionOperator
import relalg.Variable
import relalg.VariableExpression

class Cypher2RelalgUtil {

  extension RelalgFactory factory = RelalgFactory.eINSTANCE
  extension IngraphLogger logger

  new(IngraphLogger logger) {
    this.logger=logger
  }

  def Operator buildLeftDeepTree(Class<? extends BinaryOperator> binaryOperatorType,
    Iterator<Operator> i) {
    var Operator retVal = null

    // build a left deep tree of Joins from the match clauses
    if (i?.hasNext) {
      for (retVal = i.next; i.hasNext;) {
        val nextAE = switch (binaryOperatorType) {
          case typeof(JoinOperator): createJoinOperator
          case typeof(UnionOperator): createUnionOperator
          case typeof(LeftOuterJoinOperator): createLeftOuterJoinOperator
          default: {
            unsupported('''Got unexpected BinaryOperator type «binaryOperatorType.name» to build left-deep-tree''')
            null
          }
        }
        nextAE.rightInput = i.next
        nextAE.leftInput = retVal
        retVal = nextAE
      }
    }

    return retVal
  }

  def LogicalExpression buildLeftDeepTree(BinaryLogicalOperatorType binaryLogicalOperator,
    Iterator<? extends LogicalExpression> i, RelalgContainer outerContainer) {
    var LogicalExpression retVal = null

    // build a left deep tree of logical expressions with AND/OR
    if (i?.hasNext) {
      for (retVal = i.next; i.hasNext;) {
        val nextAE = createBinaryLogicalExpression => [
          operator = binaryLogicalOperator
          container = outerContainer
        ]
        nextAE.rightOperand = i.next
        nextAE.leftOperand = retVal
        retVal = nextAE
      }
    }

    return retVal
  }

  /**
   * Chain expand operators together and add sourceVertexVariables
   */
  def chainExpandOperators(GetVerticesOperator gvo, List<ExpandOperator> expandList) {
    var lastVertexVariable = gvo.vertexVariable
    var Operator lastAlgebraExpression = gvo

    for (ExpandOperator op : expandList) {
      op.sourceVertexVariable = lastVertexVariable
      op.input = lastAlgebraExpression

      lastVertexVariable = op.targetVertexVariable
      lastAlgebraExpression = op
    }

    lastAlgebraExpression
  }

  /**
   * Chain binary operators together to build a left deep tree.
   *
   * head is put on the leftInput for the 1st element of the tail list,
   * which in turn will be put on the leftInput on the 2nd element of the tail list
   * and so on.
   */
  def chainBinaryOperatorsLeft(Operator head, Iterable<? extends BinaryOperator> tail) {
    var lastAlgebraExpression = head

    for (BinaryOperator op : tail) {
      op.leftInput = lastAlgebraExpression
      lastAlgebraExpression = op
    }

    lastAlgebraExpression
  }

  /**
   * Finds the variables in expression e that are outside of grouping functions
   * and appends them to the groupingVariables set.
   *
   * This is not multithreaded-safe, so don't put it in map, forEach etc. call.
   *
   * @param e the expression to process
   * @param groupingVariables set holding the grouping variables. This set will be extended in-place.
   * @param seenAggregate indicates if previously we have seen any aggregate function
   *
   * @return boolean value indicating if there were an aggregate function. It can be expressed as: "seenAggregate || (e contains aggregate function call)"
   */
  def accumulateGroupingVariables(Expression e, Set<Variable> groupingVariables, boolean seenAggregate) {
    var effectivelySeenAggregate = seenAggregate
    val fifo = new LinkedList<Expression>

    fifo.add(e)
    while (!fifo.empty) {
      val el = fifo.removeFirst
      switch (el) {
        VariableExpression: {
          switch (myVar: el.variable) {
            GraphObjectVariable: groupingVariables.add(myVar)
            ExpressionVariable: fifo.add(myVar.expression)
          }
        }
        ArithmeticOperationExpression: {
          fifo.add(el.leftOperand)
          fifo.add(el.rightOperand)
        }
        UnaryArithmeticOperationExpression: {
          fifo.add(el.operand)
        }
        Literal: {}
        FunctionExpression: {
          if (el.functor.isAggregation) {
            effectivelySeenAggregate = true
          } else {
            fifo.addAll(el.arguments)
          }
        }
        EmptyListExpression: {}
        ListExpression: {
          fifo.add(el.head)
          fifo.add(el.tail)
        }
        default: {
          unsupported('''Unexpected, yet unsupported expression type found while enumerating grouping variables, got «el.class.name»''')
        }
      }
    }

    effectivelySeenAggregate
  }
}
