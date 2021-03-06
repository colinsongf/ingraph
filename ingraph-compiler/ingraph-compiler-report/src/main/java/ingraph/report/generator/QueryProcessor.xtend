package ingraph.report.generator

import ingraph.cypher2relalg.Cypher2Relalg
import ingraph.logger.IngraphLogger
import ingraph.relalg.calculators.ExternalSchemaCalculator
import ingraph.relalg2tex.config.RelalgConverterConfig
import ingraph.relalg2tex.converters.relalgconverters.Relalg2TexExpressionConverter
import ingraph.relalg2tex.converters.relalgconverters.Relalg2TexTreeConverter
import ingraph.report.generator.data.TestQuery
import ingraph.report.generator.util.TechReportEscaper
import java.util.List
import org.apache.commons.lang3.exception.ExceptionUtils
import org.eclipse.emf.ecore.util.EcoreUtil
import relalg.RelalgContainer
import ingraph.search2rete.Search2ReteTransformationAndSchemaCalculator
import ingraph.optimization.transformations.SearchPlanCalculator

class QueryProcessor {

	extension ExternalSchemaCalculator externalSchemaCalculator = new ExternalSchemaCalculator
	extension TechReportEscaper escaper = new TechReportEscaper
	extension IngraphLogger logger = new IngraphLogger(QueryProcessor.name)

	protected val Relalg2TexTreeConverter treeSerializer
	protected val expressionConverter = new Relalg2TexExpressionConverter

	new(RelalgConverterConfig treeSerializerConfig) {
		treeSerializer = new Relalg2TexTreeConverter(treeSerializerConfig)
	}

	def boolean toSubsection(TestQuery testQuery, List<CharSequence> subsections) {
		var RelalgContainer container = null
		try {
			container = Cypher2Relalg.processString(testQuery.querySpecification, testQuery.queryName)
		} catch (Exception e) {
			info(ExceptionUtils.getStackTrace(e))
		}
		subsections.add(subsection(container, testQuery.queryName, testQuery.querySpecification, testQuery.regressionTest))
		return container !== null
	}

	def subsection(RelalgContainer container, String name, String listing, boolean regressionTest) {
		'''
		\subsection[«name.escape»]{«name.escape»«IF (regressionTest)» \textcolor{gray}{[regression test]}«ENDIF»}

		\subsubsection*«"Query specification".toHeader(name)»

		\begin{minipage}{\linewidth}
		\begin{lstlisting}
		«listing»
		\end{lstlisting}
		\end{minipage}

		«IF container === null»
		\subsubsection*{Cannot parse query}
		Cannot parse query. This is probably a limitation in our current parser and not an error in the query specification.

		«ELSE»
		\subsubsection*«"Relational algebra expression for search-based evaluation".toHeader(name)»

		«val expression = container.expression»
		«IF expression === null»
			Cannot visualize expression.
		«ELSE»
			\begin{align*}
			\begin{autobreak}
			«expression»
			\end{autobreak}
			\end{align*}
		«ENDIF»

		\subsubsection*«"Relational algebra tree (raw)".toHeader(name)»

		«val searchTree = container.visualizeRawTree»
		«IF searchTree === null»
			Cannot visualize tree.
		«ELSE»
			\begin{center}
			\begin{adjustbox}{max width=\textwidth, max height=\textheight}
			«searchTree»
			\end{adjustbox}
			\end{center}
		«ENDIF»

		\subsubsection*«"Relational algebra tree (simplified)".toHeader(name)»

		«val simplifiedSearchTree = container.visualizeSimplifiedTree»
		«IF simplifiedSearchTree === null»
			Cannot visualize tree.
		«ELSE»
			\begin{center}
			\begin{adjustbox}{max width=\textwidth, max height=\textheight}
			«simplifiedSearchTree»
			\end{adjustbox}
			\end{center}
		«ENDIF»

		\subsubsection*«"Incremental relational algebra tree".toHeader(name)»

		«val incrementalTree = container.visualizeWithTransformations»
		«IF incrementalTree === null»
			Cannot visualize incremental tree.
		«ELSE»
			\begin{center}
			\begin{adjustbox}{max width=\textwidth, max height=\textheight}
			«incrementalTree»
			\end{adjustbox}
			\end{center}
		«ENDIF»

		«ENDIF»
		'''
	}

	def toHeader(String title, String queryName) {
		'''{«title.escape» \textcolor{gray}{(«queryName.escape»)}}'''
	}

	def expression(RelalgContainer container) {
		try {
			val expressionContainer = EcoreUtil.copy(container)
			expressionContainer.calculateExternalSchema
			expressionConverter.convert(expressionContainer).toString
		} catch (Exception e) {
			info(ExceptionUtils.getStackTrace(e))
			null
		}
	}

	def visualizeRawTree(RelalgContainer container) {
		try {
			val searchContainer = EcoreUtil.copy(container)
			val externalSchemaCalculator = new ExternalSchemaCalculator
			externalSchemaCalculator.calculateExternalSchema(searchContainer)
			treeSerializer.convert(searchContainer)
		} catch (Exception e) {
			info(ExceptionUtils.getStackTrace(e))
			null
		}
	}

	def visualizeSimplifiedTree(RelalgContainer container) {
		try {
			val searchContainer = EcoreUtil.copy(container)
			val calculator = new SearchPlanCalculator
			calculator.apply(searchContainer)
			treeSerializer.convert(searchContainer)
		} catch (Exception e) {
			info(ExceptionUtils.getStackTrace(e))
			null
		}
	}

	def visualizeWithTransformations(RelalgContainer container) {
		try {
			val incrementalContainer = EcoreUtil.copy(container)
			val calculator = new Search2ReteTransformationAndSchemaCalculator
			calculator.apply(incrementalContainer)
			treeSerializer.convert(incrementalContainer)
		} catch (Exception e) {
			info(ExceptionUtils.getStackTrace(e))
			null
		}
	}

}
