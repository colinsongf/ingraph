package ingraph.relalg.calculators

import ingraph.relalg.collectors.CollectionHelper
import java.util.List
import relalg.AttributeVariable
import relalg.BeamerOperator
import relalg.ExpressionVariable
import relalg.FunctionExpression
import relalg.GroupingAndProjectionOperator
import relalg.GroupingOperator
import relalg.ProjectionOperator
import relalg.SelectionOperator
import relalg.SortOperator
import relalg.UnaryOperator
import relalg.UnwindOperator
import relalg.Variable
import relalg.VariableExpression

class VariableExtractor {

	extension ExpressionToVariables expressionToVariables = new ExpressionToVariables
	extension CollectionHelper collectionHelper = new CollectionHelper
	extension FunctionArgumentExtractor functionArgumentExtractor = new FunctionArgumentExtractor

	/**
	 * Extract extra variables required by unary operators.
	 */
	// GroupingAndProjection-, Grouping- and ProjectionOperators
	def dispatch List<? extends Variable> extractUnaryOperatorExtraVariables(GroupingAndProjectionOperator op) {
		uniqueUnion(getExtraVariablesForGroupingOperator(op), getExtraVariablesForProjectionOperator(op))
	}

	def dispatch List<? extends Variable> extractUnaryOperatorExtraVariables(ProjectionOperator op) {
		return getExtraVariablesForProjectionOperator(op)
	}

	def dispatch List<? extends Variable> extractUnaryOperatorExtraVariables(GroupingOperator op) {
		return getExtraVariablesForGroupingOperator(op)
	}

	// SelectionOperators
	def dispatch List<? extends Variable> extractUnaryOperatorExtraVariables(SelectionOperator op) {
		getAttributes(op.condition)
	}

	// BeamerOperator (i.e. ProductionOperator)
	def dispatch List<? extends Variable> extractUnaryOperatorExtraVariables(BeamerOperator op) {
		val extraVariables = op.elements.filter(ExpressionVariable).map[expression].filter(VariableExpression).map[variable].filter(AttributeVariable).toList
		extraVariables
	}

	// SortOperator and SortAndTopOperator
	def dispatch List<? extends Variable> extractUnaryOperatorExtraVariables(SortOperator op) {
		val attributes = op.entries.map[expression].filter(VariableExpression).map[variable].filter(ExpressionVariable).map[expression].filter(VariableExpression).map[variable]
		return attributes.minus(op.basicSchema).toList
	}

	def dispatch List<? extends Variable> extractUnaryOperatorExtraVariables(UnwindOperator op) {
		#[op.element]
	}

	// rest of the unary operators, e.g. the TopOperator, require no extra attributes
	def dispatch List<AttributeVariable> extractUnaryOperatorExtraVariables(UnaryOperator op) {
		#[]
	}

	def List<? extends Variable> getExtraVariablesForProjectionOperator(ProjectionOperator op) {
		// TODO: filter out duplicates
		val functionExpressions = op.elements.map[expression].filter(FunctionExpression)
		val arguments = functionExpressions.map[extractFunctionArguments].flatten.toList

		val extraVariables = op.elements.filter(ExpressionVariable).map[expression].filter(VariableExpression).map[variable].filter(AttributeVariable).toList
		
		val aggregations = op.aggregations.map[expression].filter(FunctionExpression)		
		val aggregationExtraVariables = aggregations.map[extractFunctionArguments].flatten.toList
		

		val List<ExpressionVariable> otherFunctions = op.elements.filter[expression instanceof FunctionExpression]
			.filter[!(expression as FunctionExpression).functor.meta && !(expression as FunctionExpression).functor.aggregation]
			.toList

		op.otherFunctions.addAll(otherFunctions)
		
		uniqueUnion(extraVariables, arguments, aggregationExtraVariables)
	}
	
	def List<? extends Variable> getExtraVariablesForGroupingOperator(GroupingOperator op) {
		val basicSchemaNames = op.basicSchema.map[name]
		op.entries.filter[!basicSchemaNames.contains(it.name)].toList
	}


	def List<? extends Variable> getCalculatedVariables(ProjectionOperator op) {
		uniqueUnion(op.otherFunctions, op.aggregations)
	}

}
