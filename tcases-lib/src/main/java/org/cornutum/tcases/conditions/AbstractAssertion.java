//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.iterators.SingletonIterator;
import org.apache.commons.lang3.ObjectUtils;

import java.util.Iterator;

/**
 * Base class for {@link IAssertion assertions}.
 *
 */
public abstract class AbstractAssertion implements IAssertion
  {
  /**
   * Creates a new AbstractAssertion object.
   */
  public AbstractAssertion()
    {
    this( null);
    }
  
  /**
   * Creates a new AbstractAssertion object.
   */
  public AbstractAssertion( String property)
    {
    setProperty( property);
    }

  /**
   * Changes the property asserted.
   */
  public void setProperty( String property)
    {
    property_ = property;
    }

  /**
   * Returns the property asserted.
   */
  public String getProperty()
    {
    return property_;
    }
  
  /**
   * Returns the assertions for this disjunction.
   */
  public Iterator<IAssertion> getAssertions()
    {
    return new SingletonIterator<IAssertion>( this);
    }

  /**
   * Returns true if the given assertion is a member of this disjunction.
   */
  public boolean contains( IAssertion assertion)
    {
    return equals( assertion);
    }

  /**
   * Returns the number of assertions for this disjunction.
   */
  public int getAssertionCount()
    {
    return 1;
    }

  /**
   * Returns the disjuncts in this conjunction.
   */
  public Iterator<IDisjunct> getDisjuncts()
    {
    return new SingletonIterator<IDisjunct>( this);
    }

  /**
   * Returns the number of disjunctions for this conjunction.
   */
  public int getDisjunctCount()
    {
    return 1;
    }

  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  public abstract boolean satisfied( PropertySet properties);

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  public abstract boolean compatible( PropertySet properties);
  
  /**
   * Returns an assertion that negates this assertion.
   */
  public abstract IAssertion negate();
  
  /**
   * Returns true if this assertion negates the other.
   */
  public abstract boolean negates( IAssertion other);

  /**
   * Implements the Visitor pattern for this condition.
   */
  public abstract void accept( IConditionVisitor visitor);

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( property_)
      .toString();
    }

  @SuppressWarnings("deprecation")
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ ObjectUtils.hashCode( property_);
    }

  @SuppressWarnings("deprecation")
  public boolean equals( Object object)
    {
    return
      object != null
      && object.getClass().equals( getClass())
      && ObjectUtils.equals( ((AbstractAssertion) object).property_, property_);
    }
  
  private String property_;
  }

