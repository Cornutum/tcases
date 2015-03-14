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
  public boolean satisfied( PropertySet properties)
    {
    String property = getProperty();
    return property == null || properties.contains( property);
    }

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  public boolean compatible( PropertySet properties)
    {
    return true;
    }
  
  /**
   * Returns an assertion that negates this assertion.
   */
  public IAssertion negate()
    {
    return new AssertNot( getProperty());
    }
  
  /**
   * Returns true if this assertion negates the other.
   */
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
   * Implements the Visitor pattern for this condition.
   */
  public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }
  }

