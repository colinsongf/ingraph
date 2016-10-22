package ingraph.relalg2tex.tck

import org.junit.Test

import ingraph.cypher2relalg.Cypher2Relalg
import ingraph.relalg.util.SchemaInferencer
import ingraph.relalg2tex.RelalgTreeSerializer

class SemanticErrorAcceptanceVisualizationTest {

    val RelalgTreeSerializer serializer = new RelalgTreeSerializer
    extension SchemaInferencer inferencer = new SchemaInferencer
    
    /*
    Scenario: Handling property access on the Any type
    */
    @Test
    def void testSemanticErrorAcceptance_01() {
        val container = Cypher2Relalg.processString('''
        WITH [{prop: 0}, 1] AS list
        RETURN (list[0]).prop
        ''')
        container.addSchemaInformation
        serializer.serialize(container, "tck/SemanticErrorAcceptance_01")
    }

    /*
    Scenario: Failing when performing property access on a non-map 1
    */
    @Test
    def void testSemanticErrorAcceptance_02() {
        val container = Cypher2Relalg.processString('''
        WITH [{prop: 0}, 1] AS list
        RETURN (list[1]).prop
        ''')
        container.addSchemaInformation
        serializer.serialize(container, "tck/SemanticErrorAcceptance_02")
    }

    /*
    Scenario: Bad arguments for `range()`
    */
    @Test
    def void testSemanticErrorAcceptance_04() {
        val container = Cypher2Relalg.processString('''
        RETURN range(2, 8, 0)
        ''')
        container.addSchemaInformation
        serializer.serialize(container, "tck/SemanticErrorAcceptance_04")
    }

}