//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

/**
 * An {@link IAssertion assertion} that asserts the existence of a single property.
 *
 */
public class Assert extends AbstractAssertion
  {
  /**
   * Creates a new Assert object.
   */
  public Assert()
    {
    this( null);
    }
  
  /**
   * Creates a new Assert object.
   */
  public Assert( String property)
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
    return property == null || properties.contains( property);
    }
  
  /**
   * Returns an assertion that negates this assertion.
   */
  @Override
  public IAssertion negate()
    {
    return new AssertNot( getProperty());
    }
  
  /**
   * Returns true if this assertion negates the other.
   */
  @Override
  public boolean negates( IAssertion other)
    {
    AssertNot assertion =
      other != null && other.getClass().equals( AssertNot.class)
      ? (AssertNot) other
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

