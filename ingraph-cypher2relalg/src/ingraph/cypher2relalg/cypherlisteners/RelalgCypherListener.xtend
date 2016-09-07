package ingraph.cypher2relalg.cypherlisteners

import ingraph.antlr.CypherParser.ExpressionContext
import ingraph.antlr.CypherParser.FunctionInvocationContext
import ingraph.antlr.CypherParser.LabelNameContext
import ingraph.antlr.CypherParser.LeftArrowHeadContext
import ingraph.antlr.CypherParser.MatchContext
import ingraph.antlr.CypherParser.NodeLabelContext
import ingraph.antlr.CypherParser.NodeLabelsContext
import ingraph.antlr.CypherParser.NodePatternContext
import ingraph.antlr.CypherParser.PatternContext
import ingraph.antlr.CypherParser.PatternElementChainContext
import ingraph.antlr.CypherParser.PatternElementContext
import ingraph.antlr.CypherParser.PatternPartContext
import ingraph.antlr.CypherParser.QueryContext
import ingraph.antlr.CypherParser.RelTypeNameContext
import ingraph.antlr.CypherParser.RelationshipDetailContext
import ingraph.antlr.CypherParser.RelationshipPatternContext
import ingraph.antlr.CypherParser.RelationshipTypesContext
import ingraph.antlr.CypherParser.RelationshipsPatternContext
import ingraph.antlr.CypherParser.ReturnBodyContext
import ingraph.antlr.CypherParser.ReturnContext
import ingraph.antlr.CypherParser.ReturnItemContext
import ingraph.antlr.CypherParser.RightArrowHeadContext
import ingraph.antlr.CypherParser.SingleQueryContext
import ingraph.antlr.CypherParser.SymbolicNameContext
import ingraph.antlr.CypherParser.VariableContext
import ingraph.antlr.CypherParser.WhereContext
import ingraph.cypher2relalg.factories.EdgeLabelFactory
import ingraph.cypher2relalg.factories.EdgeVariableFactory
import ingraph.cypher2relalg.factories.VertexLabelFactory
import ingraph.cypher2relalg.factories.VertexVariableFactory
import java.util.ArrayList
import java.util.List
import org.antlr.v4.runtime.tree.TerminalNode
import org.eclipse.xtend.lib.annotations.Accessors
import relalg.AlgebraExpression
import relalg.BetaOperator
import relalg.Container
import relalg.Direction
import relalg.EdgeVariable
import relalg.ExpandOperator
import relalg.GetVerticesOperator
import relalg.JoinOperator
import relalg.Variable
import relalg.VertexVariable

class RelalgCypherListener extends RelalgBaseCypherListener {

	@Accessors(value=PUBLIC_GETTER) val Container container = createContainer

	@Accessors val vertexVariableFactory = new VertexVariableFactory(container)
	@Accessors val edgeVariableFactory = new EdgeVariableFactory(container)

	@Accessors val vertexLabelFactory = new VertexLabelFactory(container)
	@Accessors val edgeLabelFactory = new EdgeLabelFactory(container)

	/*
	 * List of single queries.
	 * 
	 * FIXME: union should be made upon multiple elements.
	 */
	var List<AlgebraExpression> query_SingleQueryList

	override enterQuery(QueryContext ctx) {
		query_SingleQueryList = new ArrayList<AlgebraExpression>
	}

	override exitQuery(QueryContext ctx) {
		val i = query_SingleQueryList.iterator
		if (i.hasNext) { // FIXME: union of multiple singleQuery's
			val rootExpression = createProductionOperator
			rootExpression.input = i.next
			
			container.rootExpression = rootExpression
		}
	}

	var List<Variable> return_VariableList = null
	var boolean return_InsideReturn = false
	var boolean return_IsDistinct = false

	override enterReturn(ReturnContext ctx) {
		return_InsideReturn = true

		if (return_VariableList == null) {
			return_VariableList = new ArrayList<Variable>
		} else {
			throw new RuntimeException("Multiple return clauses found")
		}

		return_IsDistinct = "DISTINCT".equalsIgnoreCase(ctx.getChild(TerminalNode, 1)?.text)
	}

	override exitReturn(ReturnContext ctx) {
		return_InsideReturn = false
	}

	var List<Variable> returnBody_VariableList = null
	var boolean returnBody_InsideReturnBody = false

	override enterReturnBody(ReturnBodyContext ctx) {
		returnBody_VariableList = new ArrayList<Variable>
		returnBody_InsideReturnBody = true

		if ("*".equalsIgnoreCase(ctx.returnItems.getChild(TerminalNode, 0)?.text)) {
			returnBody_VariableList.add(vertexVariableFactory.createElement("*"))
		}
	}

	override exitReturnBody(ReturnBodyContext ctx) {
		if (return_InsideReturn) {
			return_VariableList = returnBody_VariableList
		}
		returnBody_InsideReturnBody = false
	}

	override enterReturnItem(ReturnItemContext ctx) {
		// TODO: process renaming commands
		// val renamedVariableCtx = ctx.variable
		// dirty hack to traverse down to the single variable
		val returnItemName = ctx.expression?.expression12?.expression11(0)?.expression10(0)?.expression9(0)?.
			expression8?.expression7?.expression6(0).expression5(0)?.expression4(0)?.expression3?.expression2(0)?.atom?.
			variable.symbolicName.text

		if (vertexVariableFactory.elements.containsKey(returnItemName)) {
			returnBody_VariableList.add(vertexVariableFactory.createElement(returnItemName))
		} else if (edgeVariableFactory.elements.containsKey(returnItemName)) {
			returnBody_VariableList.add(edgeVariableFactory.createElement(returnItemName))
		} else {
			println("WARNING: return variable type not handled: " + returnItemName)
		}
	}

	/*
	 * List of relalg subtrees for each MATCH clause.
	 * 
	 * Should be joined together upon exitSingleQuery.
	 */
	var List<AlgebraExpression> singleQuery_MatchList

	override enterSingleQuery(SingleQueryContext ctx) {
		singleQuery_MatchList = new ArrayList<AlgebraExpression>
	}

	override exitSingleQuery(SingleQueryContext ctx) {
		val content = buildLeftDeepTree(typeof(JoinOperator), singleQuery_MatchList?.iterator)
		// add trimmer node if return was specified
		var AlgebraExpression trimmer = null
		if (return_VariableList != null) {
			trimmer = createProjectionOperator => [input = content; variables.addAll(return_VariableList)]
		} else {
			trimmer = content
		}
		// add distinct node if return DISTINCT was specified
		var AlgebraExpression distinct = null
		if (return_IsDistinct) {
			val myDistinct = createDuplicateEliminationOperator
			myDistinct.input = trimmer
			distinct = myDistinct
		} else {
			distinct = trimmer
		}
		query_SingleQueryList.add(distinct)
	}

	var AlgebraExpression match_AlgebraExpression
	var AlgebraExpression match_WhereJoinExpression
	var boolean match_WhereJoinModeIsAntijoin=false

	// TODO: where
	override enterMatch(MatchContext ctx) {
		match_AlgebraExpression = null
		match_WhereJoinExpression=null
		match_WhereJoinModeIsAntijoin=false
	}

	override exitMatch(MatchContext ctx) {
		if (match_AlgebraExpression != null) {
			var myAlgebraExpression = match_AlgebraExpression
			if (match_WhereJoinExpression != null) {
				var BetaOperator joinNode = if (match_WhereJoinModeIsAntijoin) createAntiJoinOperator else createJoinOperator
				joinNode.leftInput = myAlgebraExpression
				joinNode.rightInput = match_WhereJoinExpression
				myAlgebraExpression=joinNode
			}
			singleQuery_MatchList.add(myAlgebraExpression)
		}
	}

	var List<AlgebraExpression> pattern_PatternPartList

	override enterPattern(PatternContext ctx) {
		pattern_PatternPartList = new ArrayList<AlgebraExpression>
	}

	override exitPattern(PatternContext ctx) {
		match_AlgebraExpression = createAllDifferentOperator => [
			input = buildLeftDeepTree(typeof(JoinOperator), pattern_PatternPartList?.iterator)
		]
	}

	var AlgebraExpression patternPart_AlgebraExpression

	override enterPatternPart(PatternPartContext ctx) {
		patternPart_AlgebraExpression = null
		// FIXME: handle grammar rule: variable ws '=' ws anonymousPatternPart
	}

	override exitPatternPart(PatternPartContext ctx) {
		if (patternPart_AlgebraExpression != null) {
			pattern_PatternPartList.add(patternPart_AlgebraExpression)
		}
	}

	var List<ExpandOperator> patternElement_ExpandList
	var GetVerticesOperator patternElement_GetVerticesOperator

	override enterPatternElement(PatternElementContext ctx) {
		if (ctx.getChild(PatternElementContext, 0) == null) {
			// this is the most inner non-parenthesized patternElement, so we will process this
			patternElement_ExpandList = new ArrayList<ExpandOperator>
			patternElement_GetVerticesOperator = null
		}
	}

	override exitPatternElement(PatternElementContext ctx) {
		if (ctx.getChild(PatternElementContext, 0) == null) {
			patternPart_AlgebraExpression = chainExpandOperators(patternElement_GetVerticesOperator,
				patternElement_ExpandList)
		}
	}

	var EdgeVariable patternElementChain_EdgeVariable
	var VertexVariable patternElementChain_VertexVariable

	override enterPatternElementChain(PatternElementChainContext ctx) {
		patternElementChain_EdgeVariable = null
		patternElementChain_VertexVariable = null
	}

	override exitPatternElementChain(PatternElementChainContext ctx) {
		if (patternElementChain_EdgeVariable == null) { // variable was omitted: we create a dontCare variable
			patternElementChain_EdgeVariable = edgeVariableFactory.createElement(null)
		}
		val isLeftArrow = ctx.getChild(RelationshipPatternContext, 0)?.getChild(LeftArrowHeadContext, 0) != null
		val isRightArrow = ctx.getChild(RelationshipPatternContext, 0)?.getChild(RightArrowHeadContext, 0) != null

		val expandOperator = createExpandOperator() => [
			edgeVariable = patternElementChain_EdgeVariable;
			direction = if (isLeftArrow && isRightArrow || ! (isLeftArrow || isRightArrow))
				Direction.BOTH
			else if(isLeftArrow) Direction.IN else Direction.OUT;
			targetVertexVariable = patternElementChain_VertexVariable
		]

		switch ctx.parent {
			PatternElementContext: patternElement_ExpandList.add(expandOperator)
			RelationshipsPatternContext: relationshipsPattern_ExpandList.add(expandOperator)
		}
	}

	override enterRelationshipDetail(RelationshipDetailContext ctx) {
		val variableCtx = ctx.getChild(VariableContext, 0)
		//TODO: process possible multiple relationshipTypes
		val relationshipTypesCtx = ctx.getChild(RelationshipTypesContext, 0)

		val edgeVariableName = variableCtx?.getChild(SymbolicNameContext, 0)?.text
		val edgeVariable = edgeVariableFactory.createElement(edgeVariableName)

		if (relationshipTypesCtx != null) {
			val edgeLabelName = relationshipTypesCtx.getChild(RelTypeNameContext, 0)?.getChild(SymbolicNameContext, 0)?.
				text

			val edgeLabel = edgeLabelFactory.createElement(edgeLabelName)
			edgeVariable.ensureLabel(edgeLabel)
		}

		patternElementChain_EdgeVariable = edgeVariable
	}

	override enterNodePattern(NodePatternContext ctx) {
		val variableCtx = ctx.getChild(VariableContext, 0)
		val nodeLabelsCtx = ctx.getChild(NodeLabelsContext, 0)

		val vertexVariableName = variableCtx?.getChild(SymbolicNameContext, 0)?.text
		val vertexVariable = vertexVariableFactory.createElement(vertexVariableName)

		if (nodeLabelsCtx != null) {
			val vertexLabelName = nodeLabelsCtx.getChild(NodeLabelContext, 0)?.getChild(LabelNameContext, 0)?.getChild(
				SymbolicNameContext, 0)?.text

			val vertexLabel = vertexLabelFactory.createElement(vertexLabelName)
			vertexVariable.ensureLabel(vertexLabel)
		}

		switch ctx.parent {
			PatternElementChainContext:
				patternElementChain_VertexVariable = vertexVariable
			PatternElementContext: {
				patternElement_GetVerticesOperator = createGetVerticesOperator
				patternElement_GetVerticesOperator.vertexVariable = vertexVariable
			}
			RelationshipsPatternContext: {
				relationshipsPattern_GetVerticesOperator = createGetVerticesOperator
				relationshipsPattern_GetVerticesOperator.vertexVariable = vertexVariable
			}
		}
	}

	var boolean where_InsideWhere = false
	var AlgebraExpression where_JoinExpression
	var boolean where_JoinModeIsAntijoin=false

	override enterWhere(WhereContext ctx) {
		where_InsideWhere = true
		functionInvocation_FunctionName = null
		where_JoinExpression=null
	}

	override exitWhere(WhereContext ctx) {
		where_JoinModeIsAntijoin="NOT".equalsIgnoreCase(functionInvocation_FunctionName)
		where_InsideWhere = false

		switch ctx.parent {
			MatchContext: {
				match_WhereJoinModeIsAntijoin=where_JoinModeIsAntijoin
				match_WhereJoinExpression=where_JoinExpression
			}
		}
	}

	override enterExpression(ExpressionContext ctx) {
		super.enterExpression(ctx)
	}

	var boolean functionInvocation_InsideFunctionInvocation = false
	var String functionInvocation_FunctionName

	override enterFunctionInvocation(FunctionInvocationContext ctx) {
		functionInvocation_InsideFunctionInvocation = true
		functionInvocation_FunctionName = ctx.functionName.symbolicName.text
	}

	override exitFunctionInvocation(FunctionInvocationContext ctx) {
		functionInvocation_InsideFunctionInvocation = false
	}

	var List<ExpandOperator> relationshipsPattern_ExpandList
	var GetVerticesOperator relationshipsPattern_GetVerticesOperator

	override enterRelationshipsPattern(RelationshipsPatternContext ctx) {
		relationshipsPattern_ExpandList = new ArrayList<ExpandOperator>
	}

	override exitRelationshipsPattern(RelationshipsPatternContext ctx) {
		where_JoinExpression = chainExpandOperators(relationshipsPattern_GetVerticesOperator,
				relationshipsPattern_ExpandList)
	}
}
