//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

import java.util.Iterator;

/**
 * A {@link ICondition condition} that is satisfied if any member of a given set
 * of conditions is satisfied. Defines a logical "or" condition.
 *
 */
public class AnyOf extends ConditionSet
  {
  /**
   * Creates a new AnyOf instance.
   */
  public AnyOf( ICondition ... conditions)
    {
    super( conditions);
    }
  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  public boolean satisfied( PropertySet properties)
    {
    boolean isSatisfied;
    Iterator<ICondition> conditions;
    
    for( conditions = getConditions(),
           isSatisfied = !conditions.hasNext();

         !isSatisfied
           && conditions.hasNext();

         isSatisfied = conditions.next().satisfied( properties));
    
    return isSatisfied;
    }

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  public boolean compatible( PropertySet properties)
    {
    boolean isCompatible;
    Iterator<ICondition> conditions;
    
    for( conditions = getConditions(),
           isCompatible = !conditions.hasNext();

         !isCompatible
           && conditions.hasNext();

         isCompatible = conditions.next().compatible( properties));
    
    return isCompatible;
    }
  
  /**
   * Implements the Visitor pattern for this condition.
   */
  public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }
  }

