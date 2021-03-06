package ingraph.cypher2relalg

import ingraph.cypherparser.CypherParser
import org.slizaa.neo4j.opencypher.openCypher.Cypher
import relalg.RelalgContainer

class Cypher2Relalg {

	def static RelalgContainer processFile(String queryFile) {
		val cypher = CypherParser.parseFile(queryFile)
		return processCypher(cypher, queryFile)
	}

	@Deprecated
	def static RelalgContainer processString(String queryString) {
		processString(queryString, null)
	}

	def static RelalgContainer processString(String queryString, String queryName) {
		val cypher = CypherParser.parseString(queryString)
		return processCypher(cypher, queryName)
	}

	@Deprecated
	def static RelalgContainer processCypher(Cypher cypher) {
		processCypher(cypher, null)
	}

	def static RelalgContainer processCypher(Cypher cypher, String queryName) {
		val builder = new RelalgBuilder()
		return builder.build(cypher, queryName)
	}

}
