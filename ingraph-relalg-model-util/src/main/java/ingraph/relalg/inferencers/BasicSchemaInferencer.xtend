package ingraph.relalg.inferencers

import com.google.common.collect.ImmutableSet
import com.google.common.collect.Iterables
import com.google.common.collect.Lists
import ingraph.relalg.calculators.JoinAttributeCalculator
import ingraph.relalg.collectors.CollectionHelper
import ingraph.relalg.util.visitors.PostOrderTreeVisitor
import java.util.List
import relalg.AbstractJoinOperator
import relalg.AttributeVariable
import relalg.DualObjectSourceOperator
import relalg.ElementVariable
import relalg.ExpandOperator
import relalg.ExpressionVariable
import relalg.FunctionExpression
import relalg.GetEdgesOperator
import relalg.GetVerticesOperator
import relalg.Operator
import relalg.PathOperator
import relalg.ProjectionOperator
import relalg.RelalgContainer
import relalg.RelalgFactory
import relalg.UnaryOperator
import relalg.UnionOperator
import relalg.Variable
import relalg.VariableExpression
import relalg.function.FunctionCategory

/**
 * Infers the basic schema of the operators in the relational algebra tree.
 * 
 * This inferencing uses a postorder traversal (action are applied from the bottom to the top) 
 * first it uses recursion / dispatch methods to reach the (unary) input nodes,
 * then each method returns with the inferred schema.
 * 
 * For example, a join node concatenates the schema of its input nodes (left/right) and removes
 * duplicate attributes. 
 */
class BasicSchemaInferencer {

	extension RelalgFactory factory = RelalgFactory.eINSTANCE
	extension PostOrderTreeVisitor treeVisitor = new PostOrderTreeVisitor
	extension JoinAttributeCalculator joinAttributeCalculator = new JoinAttributeCalculator
	extension CollectionHelper collectionHelper = new CollectionHelper
	val boolean includeEdges

	new() {
		this(true)
	}

	new(boolean includeEdges) {
		this.includeEdges = includeEdges
	}

	def inferBasicSchema(RelalgContainer container) {
		if (container.basicSchemaInferred) {
			throw new IllegalStateException("BasicSchemaInferencer on relalg container was already executed")
		} else {
			container.basicSchemaInferred = true
		}

		container.rootExpression.traverse([fillBasicSchema])
		container
	}

	// nullary operators
	private def dispatch List<Variable> fillBasicSchema(DualObjectSourceOperator op) {
		op.defineBasicSchema(#[])
	}

	private def dispatch List<Variable> fillBasicSchema(GetVerticesOperator op) {
		op.defineBasicSchema(#[op.vertexVariable])
	}

	private def dispatch List<Variable> fillBasicSchema(GetEdgesOperator op) {
		if (includeEdges) {
			op.defineBasicSchema(#[op.sourceVertexVariable, op.edgeVariable, op.targetVertexVariable])
		} else {
			op.defineBasicSchema(#[op.sourceVertexVariable, op.targetVertexVariable])
		}
	}

	// unary operators
	private def dispatch List<Variable> fillBasicSchema(ProjectionOperator op) {
		val schema = op.input.basicSchema

		// check if all projected variables are in the schema
		// 1) vertices and edges
		op.elements.map[expression].filter(ElementVariable).forEach [
			if (!schema.contains(it)) {
				throw new IllegalStateException('''Variable «it.name» is not part of the schema in projection operator.''')
			}
		]
		// 2) attributes
		op.elements.map[expression].filter(AttributeVariable).forEach [
			if (!schema.contains(it.element)) {
				throw new IllegalStateException('''Attribute «it.name» cannot be projected as its vertex/edge variable does not exists.''')
			}
		]

		// extract variables
		val s = if (op.elementsToRemove.empty) {
				val aggregations = op.elements.extractAggregateFunctions
				op.aggregations.addAll(aggregations)
				op.elements.removeAll(aggregations)
				union(op.elements.extractVariables, aggregations)
			} else {
				val elementsToRemoveVariables = op.elementsToRemove.extractVariables
				val basicSchema = Lists.newArrayList(op.input.basicSchema)
				basicSchema.removeAll(elementsToRemoveVariables)
				basicSchema
			}

		op.defineBasicSchema(s)
	}

	private def extractVariables(List<ExpressionVariable> expressionVariables) {
		expressionVariables.map [ expressionVariable |
			val expression = expressionVariable.expression
			if (expression instanceof VariableExpression) {
				expression.variable
			} else { // if (expressionVariable instanceof ExpressionVariable) {
				expressionVariable
			}
		].toList
	}

	private def extractAggregateFunctions(List<ExpressionVariable> expressionVariables) {
		expressionVariables.filter[
			expression instanceof FunctionExpression &&
				(expression as FunctionExpression).functor.category == FunctionCategory.AGGREGATION
		].toList
	}

//	private def dispatch List<Variable> fillBasicSchema(GroupingAndProjectionOperator op) {
//		throw new UnsupportedOperationException("GroupingAndProjectionOperator not yet supported.")
//	}
	private def dispatch List<Variable> fillBasicSchema(ExpandOperator op) {
		val schema = Lists.newArrayList(op.input.basicSchema)

		if (includeEdges) {
			schema.add(op.edgeVariable)
		}
		schema.add(op.targetVertexVariable)
		op.defineBasicSchema(schema)
	}

	// rest of the unary operators
	private def dispatch List<Variable> fillBasicSchema(UnaryOperator op) {
		val schema = Lists.newArrayList(op.input.basicSchema)
		op.defineBasicSchema(schema)
	}

	// binary operators
	private def dispatch List<Variable> fillBasicSchema(AbstractJoinOperator op) {
		val leftInputSchema = Lists.newArrayList(op.leftInput.basicSchema)
		val rightInputSchema = Lists.newArrayList(op.rightInput.basicSchema)
		val schema = calculateJoinAttributes(op, leftInputSchema, rightInputSchema)
		op.defineBasicSchema(schema)

		// calculate common variables
		val rightSchemaNames = op.rightInput.basicSchema.map[name]
		val commonVariables = op.leftInput.basicSchema.filter[
			variable | rightSchemaNames.contains(variable.name)
		]
		op.commonVariables.addAll(commonVariables)

		op.basicSchema
	}

	private def dispatch List<Variable> fillBasicSchema(UnionOperator op) {
		// TODO I think this does the right thing but I am not sure - SzG
		if (op.leftInput.basicSchema.equals(op.rightInput.basicSchema)) {
			throw new IllegalStateException("All sub queries in a UNION must have the same column names")
		}
		// we only keep the left schema
		op.defineBasicSchema(op.leftInput.basicSchema)
	}

	// ternary operators
	private def dispatch List<Variable> fillBasicSchema(PathOperator op) {
		val schema = Lists.newArrayList(Iterables.concat(
			op.leftInput.basicSchema,
			op.middleInput.basicSchema,
			op.rightInput.basicSchema
		))

		val listExpressionVariable = createExpressionVariable => [
			expression = op.listVariable
		]

		if (includeEdges) {
			schema.add(listExpressionVariable)
		}
		schema.add(op.targetVertexVariable)
		op.defineBasicSchema(schema)
	}

	/**
	 * defineSchema
	 */
	private def defineBasicSchema(Operator op, List<Variable> basicSchema) {
		// EObjectEList.addAll() would remove duplicates anyways, 
		// but we use Guava to explicitly remove duplicates (while preserving iteration order)
		val uniqueList = ImmutableSet.copyOf(basicSchema).asList()
		op.basicSchema.addAll(uniqueList)
		basicSchema
	}

}
