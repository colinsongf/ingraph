package ingraph.relalg2tex.converters.relalgconverters

import ingraph.relalg2tex.config.RelalgConverterConfig
import relalg.BinaryOperator
import relalg.NullaryOperator
import relalg.Operator
import relalg.UnaryOperator

class Relalg2TexExpressionConverter extends AbstractRelalg2TexConverter {

	new() {
		super()
	}

	new(RelalgConverterConfig config) {
		super(config)
		if (config.textualOperators) {
			throw new UnsupportedOperationException("Textual operators are not supported for expression converters.")
		}
	}

	override convertBody(Operator expression) {
		'''
			«IF config.textualOperators»\togglefalse{textualoperators}«ENDIF»
			«IF config.standaloneDocument»$$«ENDIF»
			«children(expression)»
			«IF config.standaloneDocument»$$«ENDIF»
		'''
	}

	/**
	 * children
	 * 
	 * we add newlines intentionally to allow the autobreak package
	 * to break the expressions to multiple lines
	 */
	def dispatch CharSequence children(NullaryOperator op) {
		'''«op.operator»
		'''
	}

	def dispatch CharSequence children(UnaryOperator op) {
		'''
		«IF op.includeOperator»«op.operator»«IF config.parentheses»\Big(«ENDIF»«ENDIF»
		«op.input.children»
		«IF op.includeOperator»«IF config.parentheses»\Big)«ENDIF»«ENDIF»
		'''
	}

	def dispatch CharSequence children(BinaryOperator op) {
		'''
		«op.leftInput.children»
		«op.operator»
		«op.rightInput.children»
		'''
	}

}
