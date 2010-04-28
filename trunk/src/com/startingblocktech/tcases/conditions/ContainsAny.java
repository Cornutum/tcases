//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           Any Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.conditions;

import com.startingblocktech.tcases.ICondition;
import com.startingblocktech.tcases.PropertySet;
import com.startingblocktech.tcases.util.ToString;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A {@link ICondition condition} that is satisfied by a {@link PropertySet} that contains
 * any member of a set of properties.
 *
 * @version $Revision$, $Date$
 */
public class ContainsAny implements ICondition 
  {
  /**
   * Creates a new ContainsAny object.
   */
  public ContainsAny()
    {
    this( null);
    }

  /**
   * Creates a new ContainsAny object.
   */
  public ContainsAny( Set<String> properties)
    {
    properties_ = new HashSet<String>();
    properties_.addAll( properties);
    }

  /**
   * Adds a property to this set.
   */
  public ContainsAny add( String property)
    {
    properties_.add( property);
    return this;
    }

  /**
   * Removes a property from this set.
   */
  public ContainsAny remove( String property)
    {
    properties_.remove( property);
    return this;
    }

  /**
   * Returns true if this condition is satisfied by the given test case contains.
   */
  public boolean satisfied( PropertySet propertySet)
    {
    boolean isSatisfied;
    Iterator<String> properties;
    
    for( properties = properties_.iterator(),
           isSatisfied = properties_.isEmpty();

         !isSatisfied
           && properties.hasNext();

         isSatisfied = propertySet.contains( properties.next()));
    
    return isSatisfied;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( properties_)
      .toString();
    }

  private Set<String> properties_;
  }

