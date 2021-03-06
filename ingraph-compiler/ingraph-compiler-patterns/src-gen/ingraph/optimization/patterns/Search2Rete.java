/**
 * Generated from platform:/resource/ingraph-compiler-patterns/src/ingraph/optimization/patterns/Search2Rete.vql
 */
package ingraph.optimization.patterns;

import ingraph.optimization.patterns.DefaultExpandOperatorMatcher;
import ingraph.optimization.patterns.ExpandOperatorWithDefaultEdgeVariableMatcher;
import ingraph.optimization.patterns.GetVerticesAndExpandOperatorMatcher;
import ingraph.optimization.patterns.SortAndTopOperatorMatcher;
import ingraph.optimization.patterns.TransitiveExpandOperatorMatcher;
import ingraph.optimization.patterns.util.DefaultExpandOperatorQuerySpecification;
import ingraph.optimization.patterns.util.ExpandOperatorWithDefaultEdgeVariableQuerySpecification;
import ingraph.optimization.patterns.util.GetVerticesAndExpandOperatorQuerySpecification;
import ingraph.optimization.patterns.util.SortAndTopOperatorQuerySpecification;
import ingraph.optimization.patterns.util.TransitiveExpandOperatorQuerySpecification;
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.impl.BaseGeneratedPatternGroup;
import org.eclipse.viatra.query.runtime.exception.ViatraQueryException;

/**
 * A pattern group formed of all public patterns defined in Search2Rete.vql.
 * 
 * <p>Use the static instance as any {@link org.eclipse.viatra.query.runtime.api.IPatternGroup}, to conveniently prepare
 * a VIATRA Query engine for matching all patterns originally defined in file Search2Rete.vql,
 * in order to achieve better performance than one-by-one on-demand matcher initialization.
 * 
 * <p> From package ingraph.optimization.patterns, the group contains the definition of the following patterns: <ul>
 * <li>getVerticesAndExpandOperator</li>
 * <li>defaultExpandOperator</li>
 * <li>expandOperatorWithDefaultEdgeVariable</li>
 * <li>transitiveExpandOperator</li>
 * <li>sortAndTopOperator</li>
 * </ul>
 * 
 * @see IPatternGroup
 * 
 */
@SuppressWarnings("all")
public final class Search2Rete extends BaseGeneratedPatternGroup {
  /**
   * Access the pattern group.
   * 
   * @return the singleton instance of the group
   * @throws ViatraQueryException if there was an error loading the generated code of pattern specifications
   * 
   */
  public static Search2Rete instance() throws ViatraQueryException {
    if (INSTANCE == null) {
        INSTANCE = new Search2Rete();
    }
    return INSTANCE;
  }
  
  private static Search2Rete INSTANCE;
  
  private Search2Rete() throws ViatraQueryException {
    querySpecifications.add(GetVerticesAndExpandOperatorQuerySpecification.instance());
    querySpecifications.add(DefaultExpandOperatorQuerySpecification.instance());
    querySpecifications.add(ExpandOperatorWithDefaultEdgeVariableQuerySpecification.instance());
    querySpecifications.add(TransitiveExpandOperatorQuerySpecification.instance());
    querySpecifications.add(SortAndTopOperatorQuerySpecification.instance());
  }
  
  public GetVerticesAndExpandOperatorQuerySpecification getGetVerticesAndExpandOperator() throws ViatraQueryException {
    return GetVerticesAndExpandOperatorQuerySpecification.instance();
  }
  
  public GetVerticesAndExpandOperatorMatcher getGetVerticesAndExpandOperator(final ViatraQueryEngine engine) throws ViatraQueryException {
    return GetVerticesAndExpandOperatorMatcher.on(engine);
  }
  
  public DefaultExpandOperatorQuerySpecification getDefaultExpandOperator() throws ViatraQueryException {
    return DefaultExpandOperatorQuerySpecification.instance();
  }
  
  public DefaultExpandOperatorMatcher getDefaultExpandOperator(final ViatraQueryEngine engine) throws ViatraQueryException {
    return DefaultExpandOperatorMatcher.on(engine);
  }
  
  public ExpandOperatorWithDefaultEdgeVariableQuerySpecification getExpandOperatorWithDefaultEdgeVariable() throws ViatraQueryException {
    return ExpandOperatorWithDefaultEdgeVariableQuerySpecification.instance();
  }
  
  public ExpandOperatorWithDefaultEdgeVariableMatcher getExpandOperatorWithDefaultEdgeVariable(final ViatraQueryEngine engine) throws ViatraQueryException {
    return ExpandOperatorWithDefaultEdgeVariableMatcher.on(engine);
  }
  
  public TransitiveExpandOperatorQuerySpecification getTransitiveExpandOperator() throws ViatraQueryException {
    return TransitiveExpandOperatorQuerySpecification.instance();
  }
  
  public TransitiveExpandOperatorMatcher getTransitiveExpandOperator(final ViatraQueryEngine engine) throws ViatraQueryException {
    return TransitiveExpandOperatorMatcher.on(engine);
  }
  
  public SortAndTopOperatorQuerySpecification getSortAndTopOperator() throws ViatraQueryException {
    return SortAndTopOperatorQuerySpecification.instance();
  }
  
  public SortAndTopOperatorMatcher getSortAndTopOperator(final ViatraQueryEngine engine) throws ViatraQueryException {
    return SortAndTopOperatorMatcher.on(engine);
  }
}
