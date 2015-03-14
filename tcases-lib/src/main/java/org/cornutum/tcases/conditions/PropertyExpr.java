//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * Base class for {@link org.cornutum.tcases.conditions.ICondition conditions} that evaluate a predicate on a set of properties.
 *
 */
public abstract class PropertyExpr
  {
  /**
   * Creates a new PropertyExpr object.
   */
  protected PropertyExpr( String ... properties)
    {
    this( Arrays.asList( properties));
    }

  /**
   * Creates a new PropertyExpr object.
   */
  protected PropertyExpr( Collection<String> properties)
    {
    properties_ = new HashSet<String>();
    addProperties( properties);
    }

  /**
   * Adds a property to this set.
   */
  public void addProperty( String property)
    {
    String candidate = StringUtils.trimToNull( property);
    if( candidate != null)
      {
      properties_.add( property);
      }
    }

  /**
   * Adds a set of  properties to this set.
   */
  public void addProperties( Collection<String> properties)
    {
    for( String property : properties)
      {
      addProperty( property);
      }
    }

  /**
   * Removes a property from this set.
   */
  public void removeProperty( String property)
    {
    properties_.remove( property);
    }

  /**
   * Returns the set of properties to evaluate.
   */
  protected Iterator<String> getProperties()
    {
    return properties_.iterator();
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

