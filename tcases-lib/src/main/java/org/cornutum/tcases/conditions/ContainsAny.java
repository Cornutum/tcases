//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

import java.util.Collection;
import java.util.Iterator;

/**
 * A {@link ICondition condition} that is satisfied by a {@link PropertySet} that contains
 * any member of a set of properties.
 *
 */
public class ContainsAny extends PropertyExpr implements ICondition 
  {
  /**
   * Creates a new ContainsAny object.
   */
  public ContainsAny( String ... properties)
    {
    super( properties);
    }

  /**
   * Creates a new ContainsAny object.
   */
  public ContainsAny( Collection<String> properties)
    {
    super( properties);
    }

  /**
   * Adds a property to this set.
   */
  public ContainsAny add( String property)
    {
    super.addProperty( property);    
    return this;
    }

  /**
   * Adds a set of  properties to this set.
   */
  public ContainsAny addAll( Collection<String> properties)
    {
    super.addProperties( properties);    
    return this;
    }

  /**
   * Removes a property from this set.
   */
  public ContainsAny remove( String property)
    {
    super.removeProperty( property);    
    return this;
    }

  /**
   * Returns true if this condition is satisfied by the given test case contains.
   */
  @Override
  public boolean satisfied( PropertySet propertySet)
    {
    boolean isSatisfied;
    Iterator<String> properties;
    
    for( properties = getProperties(),
           isSatisfied = !properties.hasNext();

         !isSatisfied
           && properties.hasNext();

         isSatisfied = propertySet.contains( properties.next()));
    
    return isSatisfied;
    }

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  @Override
  public boolean compatible( PropertySet properties)
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

