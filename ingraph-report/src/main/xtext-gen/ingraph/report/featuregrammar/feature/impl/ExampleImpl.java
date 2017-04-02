/**
 * generated by Xtext 2.10.0
 */
package ingraph.report.featuregrammar.feature.impl;

import ingraph.report.featuregrammar.feature.Example;
import ingraph.report.featuregrammar.feature.ExampleRow;
import ingraph.report.featuregrammar.feature.FeaturePackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Example</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link ingraph.report.featuregrammar.feature.impl.ExampleImpl#getHeading <em>Heading</em>}</li>
 *   <li>{@link ingraph.report.featuregrammar.feature.impl.ExampleImpl#getRows <em>Rows</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ExampleImpl extends MinimalEObjectImpl.Container implements Example
{
  /**
   * The cached value of the '{@link #getHeading() <em>Heading</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getHeading()
   * @generated
   * @ordered
   */
  protected ExampleRow heading;

  /**
   * The cached value of the '{@link #getRows() <em>Rows</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRows()
   * @generated
   * @ordered
   */
  protected EList<ExampleRow> rows;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ExampleImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return FeaturePackage.Literals.EXAMPLE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ExampleRow getHeading()
  {
    return heading;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetHeading(ExampleRow newHeading, NotificationChain msgs)
  {
    ExampleRow oldHeading = heading;
    heading = newHeading;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, FeaturePackage.EXAMPLE__HEADING, oldHeading, newHeading);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setHeading(ExampleRow newHeading)
  {
    if (newHeading != heading)
    {
      NotificationChain msgs = null;
      if (heading != null)
        msgs = ((InternalEObject)heading).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - FeaturePackage.EXAMPLE__HEADING, null, msgs);
      if (newHeading != null)
        msgs = ((InternalEObject)newHeading).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - FeaturePackage.EXAMPLE__HEADING, null, msgs);
      msgs = basicSetHeading(newHeading, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, FeaturePackage.EXAMPLE__HEADING, newHeading, newHeading));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ExampleRow> getRows()
  {
    if (rows == null)
    {
      rows = new EObjectContainmentEList<ExampleRow>(ExampleRow.class, this, FeaturePackage.EXAMPLE__ROWS);
    }
    return rows;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case FeaturePackage.EXAMPLE__HEADING:
        return basicSetHeading(null, msgs);
      case FeaturePackage.EXAMPLE__ROWS:
        return ((InternalEList<?>)getRows()).basicRemove(otherEnd, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case FeaturePackage.EXAMPLE__HEADING:
        return getHeading();
      case FeaturePackage.EXAMPLE__ROWS:
        return getRows();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case FeaturePackage.EXAMPLE__HEADING:
        setHeading((ExampleRow)newValue);
        return;
      case FeaturePackage.EXAMPLE__ROWS:
        getRows().clear();
        getRows().addAll((Collection<? extends ExampleRow>)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case FeaturePackage.EXAMPLE__HEADING:
        setHeading((ExampleRow)null);
        return;
      case FeaturePackage.EXAMPLE__ROWS:
        getRows().clear();
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case FeaturePackage.EXAMPLE__HEADING:
        return heading != null;
      case FeaturePackage.EXAMPLE__ROWS:
        return rows != null && !rows.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //ExampleImpl