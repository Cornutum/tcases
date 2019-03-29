//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

/**
 * An {@link IAssertion assertion} that the number of instances of a property does not exceed a specified maximum.
 *
 */
public class AssertNotMore extends BoundedAssertion
  {
  /**
   * Creates a new AssertNotMore object.
   */
  public AssertNotMore()
    {
    this( null, 0);
    }
  
  /**
   * Creates a new AssertNotMore object.
   */
  public AssertNotMore( String property, int maximum)
    {
    super( property, maximum);
    }

  /**
   * Returns true is the bound is exclusive.
   */
  public boolean isExclusive()
    {
    return false;
    }

  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  public boolean satisfied( PropertySet properties)
    {
    return properties.getCount( getProperty()) <= getBound();
    }
  
  /**
   * Returns an assertion that negates this assertion.
   */
  public IAssertion negate()
    {
    return new AssertMore( getProperty(), getBound());
    }
  
  /**
   * Returns true if this assertion negates the other.
   */
  public boolean negates( IAssertion other)
    {
    AssertMore assertion =
      other != null && other.getClass().equals( AssertMore.class)
      ? (AssertMore) other
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

