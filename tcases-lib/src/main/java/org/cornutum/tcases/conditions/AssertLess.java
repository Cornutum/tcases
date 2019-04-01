//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

/**
 * An {@link IAssertion assertion} that the number of instances of a property is less than a specified maximum.
 *
 */
public class AssertLess extends BoundedAssertion
  {
  /**
   * Creates a new AssertLess object.
   */
  public AssertLess()
    {
    this( null, 0);
    }
  
  /**
   * Creates a new AssertLess object.
   */
  public AssertLess( String property, int maximum)
    {
    super( property, maximum);
    }

  /**
   * Returns true is the bound is exclusive.
   */
  public boolean isExclusive()
    {
    return true;
    }

  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  public boolean satisfied( PropertySet properties)
    {
    return properties.getCount( getProperty()) < getBound();
    }
  
  /**
   * Returns an assertion that negates this assertion.
   */
  public IAssertion negate()
    {
    return new AssertNotLess( getProperty(), getBound());
    }
  
  /**
   * Returns true if this assertion negates the other.
   */
  public boolean negates( IAssertion other)
    {
    AssertNotLess assertion =
      other != null && other.getClass().equals( AssertNotLess.class)
      ? (AssertNotLess) other
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
  public boolean completable()
    {
    return false;
    }
  
  /**
   * Implements the Visitor pattern for this condition.
   */
  public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }
  }

