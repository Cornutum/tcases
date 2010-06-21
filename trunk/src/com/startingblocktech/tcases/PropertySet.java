//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.collections15.bag.HashBag;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

/**
 * Defines a set of test case properties.
 *
 * @version $Revision$, $Date$
 */
public class PropertySet
  {
  /**
   * Creates a new PropertySet object.
   */
  public PropertySet()
    {
    this( (Collection<String>) null);
    }
  
  /**
   * Creates a new PropertySet object.
   */
  public PropertySet( String ...properties)
    {
    this( Arrays.asList( properties));
    }
  
  /**
   * Creates a new PropertySet object.
   */
  public PropertySet( Collection<String> properties)
    {
    properties_ = new HashBag<String>();
    addAll( properties);
    }

  /**
   * Adds a property to this set.
   */
  public PropertySet add( String property)
    {
    String candidate = StringUtils.trimToNull( property);
    if( candidate != null)
      {
      properties_.add( candidate);
      }

    return this;
    }

  /**
   * Adds a set of properties to this set.
   */
  public PropertySet addAll( PropertySet propertySet)
    {
    if( propertySet != null)
      {
      addAll( propertySet.properties_.uniqueSet());
      }

    return this;
    }

  /**
   * Adds a set of properties to this set.
   */
  public PropertySet addAll( Collection<String> properties)
    {
    if( properties != null)
      {
      for( String property : properties)
        {
        add( property);
        }
      }

    return this;
    }

  /**
   * Removes a property from this set.
   */
  public PropertySet remove( String property)
    {
    String candidate = StringUtils.trimToNull( property);
    if( candidate != null)
      {
      properties_.remove( candidate, 1);
      }

    return this;
    }

  /**
   * Removes a set of properties from this set.
   */
  public PropertySet removeAll( Collection<String> properties)
    {
    if( properties != null)
      {
      for( String property : properties)
        {
        remove( property);
        }
      }
    
    return this;
    }

  /**
   * Removes all properties this set.
   */
  public void clear()
    {
    properties_.clear();
    }

  /**
   * Returns true if the given property is a member of this set.
   */
  public boolean contains( String property)
    {
    return properties_.contains( property);
    }

  /**
   * Returns the members of this set.
   */
  public Iterator<String> getProperties()
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

  private HashBag<String> properties_;
  }

