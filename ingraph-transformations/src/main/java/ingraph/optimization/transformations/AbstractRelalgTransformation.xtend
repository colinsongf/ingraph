package ingraph.optimization.transformations

import org.apache.log4j.Level
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.emf.EMFScope
import org.eclipse.viatra.query.runtime.util.ViatraQueryLoggingUtil
import org.eclipse.viatra.transformation.runtime.emf.rules.batch.BatchTransformationRuleFactory
import org.eclipse.viatra.transformation.runtime.emf.transformation.batch.BatchTransformation
import org.eclipse.xtext.EcoreUtil2
import relalg.BinaryOperator
import relalg.ExpandOperator
import relalg.Operator
import relalg.RelalgContainer
import relalg.RelalgFactory
import relalg.UnaryOperator
import java.io.Closeable
import java.io.IOException
import relalg.Direction

abstract class AbstractRelalgTransformation implements Closeable {

	protected val extension RelalgFactory relalgFactory = RelalgFactory.eINSTANCE
	protected val extension BatchTransformationRuleFactory ruleFactory = new BatchTransformationRuleFactory

	var AdvancedViatraQueryEngine engine

	new() {
		ViatraQueryLoggingUtil.getDefaultLogger().setLevel(Level.OFF)
	}

	protected def register(RelalgContainer container) {
		val scope = new EMFScope(container)
		engine = AdvancedViatraQueryEngine.createUnmanagedEngine(scope)

		val transformation = BatchTransformation.forEngine(engine).build
		val statements = transformation.transformationStatements
		return statements
	}

	protected def void changeChildOperator(Operator parentOperator, Operator currentOperator, Operator newOperator) {
		switch parentOperator {
			UnaryOperator: {
				parentOperator.input = newOperator
			}
			BinaryOperator: {
				if (parentOperator.getLeftInput.equals(currentOperator)) {
					parentOperator.leftInput = newOperator
				}
				if (parentOperator.getRightInput.equals(currentOperator)) {
					parentOperator.rightInput = newOperator
				}
			}
		}
		
		if (currentOperator instanceof ExpandOperator) {
			EcoreUtil2.delete(currentOperator)
		}
	}

	protected def source(ExpandOperator expandOperator) {
		switch (expandOperator.direction) {
			case BOTH,
			case IN: {
				return expandOperator.targetVertexVariable
			}
			case OUT: {
				return expandOperator.sourceVertexVariable
			}
		}
	}

	protected def target(ExpandOperator expandOperator) {
		switch (expandOperator.direction) {
			case BOTH,
			case IN: {
				return expandOperator.sourceVertexVariable
			}
			case OUT: {
				return expandOperator.targetVertexVariable
			}
		}
	}

	override close() throws IOException {
		engine?.dispose
	}

	protected def normalizeDirection(Direction direction) {
		switch (direction) {
			case BOTH: {
				return Direction.BOTH
			}
			default: {
				return Direction.OUT
			}
		}
	}	
}
