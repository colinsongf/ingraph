package ingraph.expressionparser

import org.scalatest.WordSpec

import ingraph.relalg.util.SchemaToMap
import relalg._
import ingraph.cypher2relalg.Cypher2Relalg
import ingraph.search2rete.Search2ReteTransformationAndSchemaCalculator

class ExpressionParserTest extends WordSpec {
  def getSelectionOperator(query: String): SelectionOperator = {
    val reteCalc = new Search2ReteTransformationAndSchemaCalculator
    val relalg = reteCalc.apply(Cypher2Relalg.processString(query))
    relalg.getRootExpression
      .asInstanceOf[ProductionOperator].getInput
      .asInstanceOf[ProjectionOperator].getInput
      .asInstanceOf[SelectionOperator]
  }
  import Conversions._

  val schemaToMap = new SchemaToMap()
  import scala.collection.JavaConverters._
  def getSchema(operator: Operator): Map[String, Integer] = schemaToMap.schemaToMapNames(operator).asScala.toMap

  val rocks = "emfrocks"
  "ExpressionParser" should {
    "parse equal to" in {
      val op = getSelectionOperator("""MATCH (n) WHERE n.something="emfsucks" RETURN n.something""")
      val lookup = getSchema(op.getInput)
      val func = ExpressionParser.parse(op.getCondition, lookup)
      assert(func(Vector(0, "emfsucks")))
      assert(!func(Vector(1, rocks)))
    }

   "parse binary logical expressions" in {
      val op = getSelectionOperator("""MATCH (n) WHERE (1=1 and n=2) or (n="emfrocks" xor 2=3) RETURN n""")
      val lookup = getSchema(op.getInput)
      val func = ExpressionParser.parse(op.getCondition, lookup)
      assert(func(Vector(2)))
      assert(!func(Vector(1)))
      assert(func(Vector(rocks)))
      assert(!func(Vector(s"not $rocks")))
    }

    "parse unary logical expressions" in {
      val op = getSelectionOperator("""MATCH (n) WHERE not n=2 RETURN n""")
      val lookup = getSchema(op.getInput)
      val func = ExpressionParser.parse(op.getCondition, lookup)
      assert(!func(Vector(2)))
      assert(func(Vector(1)))
    }

    "parse arithmetic operations" in {
      val op = getSelectionOperator("""MATCH (n) WHERE n=2^2*1+2-4%5 RETURN n""")
      val lookup = getSchema(op.getInput)
      val func = ExpressionParser.parse(op.getCondition, lookup)
      assert(func(Vector(2)))
      assert(!func(Vector(1)))
    }

    "parse complicated routesensor expression" ignore {
      val op = getSelectionOperator("""MATCH (n) WHERE n.a is null and n.e is not null RETURN n""")
      val lookup = getSchema(op.getInput)
      val func = ExpressionParser.parse(op.getCondition, lookup)
      assert(func(Vector(null, 1)))
      assert(!func(Vector(1, 1)))
    }

    "parse functions" in {
      val op = getSelectionOperator("""MATCH (n) WHERE cos(toFloat(n)) > sin(pi()) RETURN n""")
      val lookup = getSchema(op.getInput)
      val func = ExpressionParser.parse(op.getCondition, lookup)
      assert(func(Vector(0)))
    }

    "parse string functions" in {
      val op = getSelectionOperator(
        """MATCH (n) WHERE substring(toString(1324), 2)=left("244", 2) RETURN n""")
      val lookup = getSchema(op.getInput)
      val func = ExpressionParser.parse(op.getCondition, lookup)
      assert(func(Vector(24)))
    }
  }
}
