//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

/**
 * An {@link IAssertion assertion} that asserts the non-existence of a single property.
 *
 */
public class AssertNot extends AbstractAssertion
  {
  /**
   * Creates a new AssertNot object.
   */
  public AssertNot()
    {
    this( null);
    }
  
  /**
   * Creates a new AssertNot object.
   */
  public AssertNot( String property)
    {
    super( property);
    }

  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  @Override
  public boolean satisfied( PropertySet properties)
    {
    String property = getProperty();
    return !(property != null && properties.contains( property));
    }
  
  /**
   * Returns an assertion that negates this assertion.
   */
  @Override
  public IAssertion negate()
    {
    return new Assert( getProperty());
    }
  
  /**
   * Returns true if this assertion negates the other.
   */
  @Override
  public boolean negates( IAssertion other)
    {
    Assert assertion =
      other != null && other.getClass().equals( Assert.class)
      ? (Assert) other
      : null;

    return
      assertion != null
      && assertion.getProperty().equals( getProperty());
    }

  /**
   * Returns true if any property set that does NOT satisfy this assertion can be made to satisfy this assertion by
   * the addition of another (instance of a) property.
   */
  @Override
  public boolean completable()
    {
    return false;
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

