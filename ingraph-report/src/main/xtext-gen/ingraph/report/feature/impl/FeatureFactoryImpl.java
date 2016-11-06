/**
 * generated by Xtext 2.10.0
 */
package ingraph.report.feature.impl;

import ingraph.report.feature.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class FeatureFactoryImpl extends EFactoryImpl implements FeatureFactory
{
  /**
   * Creates the default factory implementation.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static FeatureFactory init()
  {
    try
    {
      FeatureFactory theFeatureFactory = (FeatureFactory)EPackage.Registry.INSTANCE.getEFactory(FeaturePackage.eNS_URI);
      if (theFeatureFactory != null)
      {
        return theFeatureFactory;
      }
    }
    catch (Exception exception)
    {
      EcorePlugin.INSTANCE.log(exception);
    }
    return new FeatureFactoryImpl();
  }

  /**
   * Creates an instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public FeatureFactoryImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public EObject create(EClass eClass)
  {
    switch (eClass.getClassifierID())
    {
      case FeaturePackage.FEATURE: return createFeature();
      case FeaturePackage.NARRATIVE_ELEMENT: return createNarrativeElement();
      case FeaturePackage.FREE_TEXT: return createFreeText();
      case FeaturePackage.AS_A: return createAsA();
      case FeaturePackage.IN_ORDER_TO: return createInOrderTo();
      case FeaturePackage.IWANT_TO: return createIWantTo();
      case FeaturePackage.ABSTRACT_SCENARIO: return createAbstractScenario();
      case FeaturePackage.SCENARIO: return createScenario();
      case FeaturePackage.SCENARIO_WITH_OUTLINE: return createScenarioWithOutline();
      case FeaturePackage.EXAMPLE: return createExample();
      case FeaturePackage.EXAMPLE_ROW: return createExampleRow();
      case FeaturePackage.EXAMPLE_CELL: return createExampleCell();
      case FeaturePackage.BACKGROUND: return createBackground();
      case FeaturePackage.STEP: return createStep();
      case FeaturePackage.GIVEN_STEP: return createGivenStep();
      case FeaturePackage.WHEN_STEP: return createWhenStep();
      case FeaturePackage.THEN_STEP: return createThenStep();
      case FeaturePackage.AND_STEP: return createAndStep();
      default:
        throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Feature createFeature()
  {
    FeatureImpl feature = new FeatureImpl();
    return feature;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NarrativeElement createNarrativeElement()
  {
    NarrativeElementImpl narrativeElement = new NarrativeElementImpl();
    return narrativeElement;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public FreeText createFreeText()
  {
    FreeTextImpl freeText = new FreeTextImpl();
    return freeText;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AsA createAsA()
  {
    AsAImpl asA = new AsAImpl();
    return asA;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public InOrderTo createInOrderTo()
  {
    InOrderToImpl inOrderTo = new InOrderToImpl();
    return inOrderTo;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IWantTo createIWantTo()
  {
    IWantToImpl iWantTo = new IWantToImpl();
    return iWantTo;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AbstractScenario createAbstractScenario()
  {
    AbstractScenarioImpl abstractScenario = new AbstractScenarioImpl();
    return abstractScenario;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Scenario createScenario()
  {
    ScenarioImpl scenario = new ScenarioImpl();
    return scenario;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ScenarioWithOutline createScenarioWithOutline()
  {
    ScenarioWithOutlineImpl scenarioWithOutline = new ScenarioWithOutlineImpl();
    return scenarioWithOutline;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Example createExample()
  {
    ExampleImpl example = new ExampleImpl();
    return example;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExampleRow createExampleRow()
  {
    ExampleRowImpl exampleRow = new ExampleRowImpl();
    return exampleRow;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExampleCell createExampleCell()
  {
    ExampleCellImpl exampleCell = new ExampleCellImpl();
    return exampleCell;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Background createBackground()
  {
    BackgroundImpl background = new BackgroundImpl();
    return background;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Step createStep()
  {
    StepImpl step = new StepImpl();
    return step;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public GivenStep createGivenStep()
  {
    GivenStepImpl givenStep = new GivenStepImpl();
    return givenStep;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public WhenStep createWhenStep()
  {
    WhenStepImpl whenStep = new WhenStepImpl();
    return whenStep;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ThenStep createThenStep()
  {
    ThenStepImpl thenStep = new ThenStepImpl();
    return thenStep;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public AndStep createAndStep()
  {
    AndStepImpl andStep = new AndStepImpl();
    return andStep;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public FeaturePackage getFeaturePackage()
  {
    return (FeaturePackage)getEPackage();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @deprecated
   * @generated
   */
  @Deprecated
  public static FeaturePackage getPackage()
  {
    return FeaturePackage.eINSTANCE;
  }

} //FeatureFactoryImpl