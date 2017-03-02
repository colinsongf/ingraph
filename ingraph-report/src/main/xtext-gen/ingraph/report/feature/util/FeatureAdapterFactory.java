/**
 * generated by Xtext 2.10.0
 */
package ingraph.report.feature.util;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

import ingraph.report.feature.AbstractScenario;
import ingraph.report.feature.AndStep;
import ingraph.report.feature.AsA;
import ingraph.report.feature.Background;
import ingraph.report.feature.Example;
import ingraph.report.feature.ExampleCell;
import ingraph.report.feature.ExampleRow;
import ingraph.report.feature.Feature;
import ingraph.report.feature.FeaturePackage;
import ingraph.report.feature.FreeText;
import ingraph.report.feature.GivenStep;
import ingraph.report.feature.IWantTo;
import ingraph.report.feature.InOrderTo;
import ingraph.report.feature.NarrativeElement;
import ingraph.report.feature.Scenario;
import ingraph.report.feature.ScenarioWithOutline;
import ingraph.report.feature.Step;
import ingraph.report.feature.ThenStep;
import ingraph.report.feature.WhenStep;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see ingraph.report.feature.FeaturePackage
 * @generated
 */
public class FeatureAdapterFactory extends AdapterFactoryImpl
{
  /**
   * The cached model package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static FeaturePackage modelPackage;

  /**
   * Creates an instance of the adapter factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public FeatureAdapterFactory()
  {
    if (modelPackage == null)
    {
      modelPackage = FeaturePackage.eINSTANCE;
    }
  }

  /**
   * Returns whether this factory is applicable for the type of the object.
   * <!-- begin-user-doc -->
   * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
   * <!-- end-user-doc -->
   * @return whether this factory is applicable for the type of the object.
   * @generated
   */
  @Override
  public boolean isFactoryForType(Object object)
  {
    if (object == modelPackage)
    {
      return true;
    }
    if (object instanceof EObject)
    {
      return ((EObject)object).eClass().getEPackage() == modelPackage;
    }
    return false;
  }

  /**
   * The switch that delegates to the <code>createXXX</code> methods.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected FeatureSwitch<Adapter> modelSwitch =
    new FeatureSwitch<Adapter>()
    {
      @Override
      public Adapter caseFeature(Feature object)
      {
        return createFeatureAdapter();
      }
      @Override
      public Adapter caseNarrativeElement(NarrativeElement object)
      {
        return createNarrativeElementAdapter();
      }
      @Override
      public Adapter caseFreeText(FreeText object)
      {
        return createFreeTextAdapter();
      }
      @Override
      public Adapter caseAsA(AsA object)
      {
        return createAsAAdapter();
      }
      @Override
      public Adapter caseInOrderTo(InOrderTo object)
      {
        return createInOrderToAdapter();
      }
      @Override
      public Adapter caseIWantTo(IWantTo object)
      {
        return createIWantToAdapter();
      }
      @Override
      public Adapter caseAbstractScenario(AbstractScenario object)
      {
        return createAbstractScenarioAdapter();
      }
      @Override
      public Adapter caseScenario(Scenario object)
      {
        return createScenarioAdapter();
      }
      @Override
      public Adapter caseScenarioWithOutline(ScenarioWithOutline object)
      {
        return createScenarioWithOutlineAdapter();
      }
      @Override
      public Adapter caseExample(Example object)
      {
        return createExampleAdapter();
      }
      @Override
      public Adapter caseExampleRow(ExampleRow object)
      {
        return createExampleRowAdapter();
      }
      @Override
      public Adapter caseExampleCell(ExampleCell object)
      {
        return createExampleCellAdapter();
      }
      @Override
      public Adapter caseBackground(Background object)
      {
        return createBackgroundAdapter();
      }
      @Override
      public Adapter caseStep(Step object)
      {
        return createStepAdapter();
      }
      @Override
      public Adapter caseGivenStep(GivenStep object)
      {
        return createGivenStepAdapter();
      }
      @Override
      public Adapter caseWhenStep(WhenStep object)
      {
        return createWhenStepAdapter();
      }
      @Override
      public Adapter caseThenStep(ThenStep object)
      {
        return createThenStepAdapter();
      }
      @Override
      public Adapter caseAndStep(AndStep object)
      {
        return createAndStepAdapter();
      }
      @Override
      public Adapter defaultCase(EObject object)
      {
        return createEObjectAdapter();
      }
    };

  /**
   * Creates an adapter for the <code>target</code>.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param target the object to adapt.
   * @return the adapter for the <code>target</code>.
   * @generated
   */
  @Override
  public Adapter createAdapter(Notifier target)
  {
    return modelSwitch.doSwitch((EObject)target);
  }


  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.Feature <em>Feature</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.Feature
   * @generated
   */
  public Adapter createFeatureAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.NarrativeElement <em>Narrative Element</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.NarrativeElement
   * @generated
   */
  public Adapter createNarrativeElementAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.FreeText <em>Free Text</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.FreeText
   * @generated
   */
  public Adapter createFreeTextAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.AsA <em>As A</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.AsA
   * @generated
   */
  public Adapter createAsAAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.InOrderTo <em>In Order To</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.InOrderTo
   * @generated
   */
  public Adapter createInOrderToAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.IWantTo <em>IWant To</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.IWantTo
   * @generated
   */
  public Adapter createIWantToAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.AbstractScenario <em>Abstract Scenario</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.AbstractScenario
   * @generated
   */
  public Adapter createAbstractScenarioAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.Scenario <em>Scenario</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.Scenario
   * @generated
   */
  public Adapter createScenarioAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.ScenarioWithOutline <em>Scenario With Outline</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.ScenarioWithOutline
   * @generated
   */
  public Adapter createScenarioWithOutlineAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.Example <em>Example</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.Example
   * @generated
   */
  public Adapter createExampleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.ExampleRow <em>Example Row</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.ExampleRow
   * @generated
   */
  public Adapter createExampleRowAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.ExampleCell <em>Example Cell</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.ExampleCell
   * @generated
   */
  public Adapter createExampleCellAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.Background <em>Background</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.Background
   * @generated
   */
  public Adapter createBackgroundAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.Step <em>Step</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.Step
   * @generated
   */
  public Adapter createStepAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.GivenStep <em>Given Step</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.GivenStep
   * @generated
   */
  public Adapter createGivenStepAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.WhenStep <em>When Step</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.WhenStep
   * @generated
   */
  public Adapter createWhenStepAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.ThenStep <em>Then Step</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.ThenStep
   * @generated
   */
  public Adapter createThenStepAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link ingraph.report.feature.AndStep <em>And Step</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see ingraph.report.feature.AndStep
   * @generated
   */
  public Adapter createAndStepAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for the default case.
   * <!-- begin-user-doc -->
   * This default implementation returns null.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @generated
   */
  public Adapter createEObjectAdapter()
  {
    return null;
  }

} //FeatureAdapterFactory
