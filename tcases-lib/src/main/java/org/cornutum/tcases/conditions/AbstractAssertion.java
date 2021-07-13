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

import java.util.Iterator;
import java.util.Objects;

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
  @Override
public String getProperty()
    {
    return property_;
    }
  
  /**
   * Returns the assertions for this disjunction.
   */
  @Override
public Iterator<IAssertion> getAssertions()
    {
    return new SingletonIterator<IAssertion>( this);
    }

  /**
   * Returns true if the given assertion is a member of this disjunction.
   */
  @Override
public boolean contains( IAssertion assertion)
    {
    return equals( assertion);
    }

  /**
   * Returns the number of assertions for this disjunction.
   */
  @Override
public int getAssertionCount()
    {
    return 1;
    }

  /**
   * Returns the disjuncts in this conjunction.
   */
  @Override
public Iterator<IDisjunct> getDisjuncts()
    {
    return new SingletonIterator<IDisjunct>( this);
    }

  /**
   * Returns the number of disjunctions for this conjunction.
   */
  @Override
public int getDisjunctCount()
    {
    return 1;
    }

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  @Override
public boolean compatible( PropertySet properties)
    {
    return completable() || satisfied( properties);
    }

  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( property_)
      .toString();
    }

  @Override
public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( property_);
    }

  @Override
public boolean equals( Object object)
    {
    return
      object != null
      && object.getClass().equals( getClass())
      && Objects.equals( ((AbstractAssertion) object).property_, property_);
    }
  
  private String property_;
  }

