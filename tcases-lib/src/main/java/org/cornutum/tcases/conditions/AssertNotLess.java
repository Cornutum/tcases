//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

/**
 * An {@link IAssertion assertion} that the number of instances of a property is not less than a specified minimum.
 *
 */
public class AssertNotLess extends BoundedAssertion
  {
  /**
   * Creates a new AssertNotLess object.
   */
  public AssertNotLess()
    {
    this( null, 0);
    }
  
  /**
   * Creates a new AssertNotLess object.
   */
  public AssertNotLess( String property, int minimum)
    {
    super( property, minimum);
    }

  /**
   * Returns true is the bound is exclusive.
   */
  @Override
public boolean isExclusive()
    {
    return false;
    }

  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  @Override
public boolean satisfied( PropertySet properties)
    {
    return properties.getCount( getProperty()) >= getBound();
    }
  
  /**
   * Returns an assertion that negates this assertion.
   */
  @Override
public IAssertion negate()
    {
    return new AssertLess( getProperty(), getBound());
    }
  
  /**
   * Returns true if this assertion negates the other.
   */
  @Override
public boolean negates( IAssertion other)
    {
    AssertLess assertion =
      other != null && other.getClass().equals( AssertLess.class)
      ? (AssertLess) other
      : null;

    return
      assertion != null
      && assertion.getProperty().equals( getProperty())
      && assertion.getBound() == getBound();
    }

  /**
   * Returns true if any property set that does NOT satisfy this assertion can be made to satisfy this assertion by
   * the addition of another (instance of a) property.
   */
  @Override
public boolean completable()
    {
    return true;
    }
  
  /**
   * Implements the Visitor pattern for this condition.
   */
  @Override
public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }
  }

