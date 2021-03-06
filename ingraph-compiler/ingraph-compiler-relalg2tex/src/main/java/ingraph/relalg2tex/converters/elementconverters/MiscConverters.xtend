package ingraph.relalg2tex.converters.elementconverters

import java.util.List
import relalg.AbstractEdgeVariable
import relalg.Direction
import relalg.MaxHops
import relalg.SortEntry

class MiscConverters {

	extension StringEscaper stringEscaper = new StringEscaper
	extension ExpressionConverter expressionConverter = new ExpressionConverter

	def convertDirection(Direction direction) {
		switch direction {
			case BOTH: '''both'''
			case IN: '''in'''
			case OUT: '''out'''
			default: throw new UnsupportedOperationException('''Direction «direction» not supported.''')
		}
	}

	def convertConditionString(String s) {
		s.escape //
			.replaceAll(''' XOR ''', ''' \\lxor ''') //
			.replaceAll(''' AND ''', ''' \\land ''') //
			.replaceAll(''' OR ''', ''' \\lor ''') //
			.replaceAll(''' ''', '''\ ''') //
	}

	def CharSequence hopsToString(MaxHops hops) {
		switch hops.maxHopsType {
			case LIMITED: hops.hops.toString
			case UNLIMITED: '''\infty'''
			default: throw new UnsupportedOperationException('''MaxHopsType «hops.maxHopsType» not supported.''')
		}
	}

	def sortEntryToTex(SortEntry entry) {
		val direction = switch (entry.direction) {
			case ASCENDING: "asc"
			case DESCENDING: "desc"
			default: throw new UnsupportedOperationException('''SortEntry «entry.direction» not supported.''')
		}
		'''\«direction» «entry.expression.convertExpression»'''
	}

	def edgeVariableList(List<AbstractEdgeVariable> edgeVariables) {
		'''«edgeVariables.map["\\var{"+ escapedName + "}"].join(", ")»'''
	}

}
