//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.collections15.bag.HashBag;
import org.apache.commons.lang.StringUtils;

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
    this( null);
    }
  
  /**
   * Creates a new PropertySet object.
   */
  public PropertySet( String[] properties)
    {
    properties_ = new HashBag<String>();
    if( properties != null)
      {
      for( int i = 0; i < properties.length; i++)
        {
        String property = StringUtils.trimToNull( properties[i]);
        if( property == null)
          {
          add( property);
          }
        }
      }
    }

  /**
   * Adds a property to this set.
   */
  public PropertySet add( String property)
    {
    assert property != null;
    properties_.add( property);
    return this;
    }

  /**
   * Adds a set of properties to this set.
   */
  public PropertySet addAll( PropertySet propertySet)
    {
    if( propertySet != null)
      {
      properties_.addAll( propertySet.properties_.uniqueSet());
      }

    return this;
    }

  /**
   * Removes a property from this set.
   */
  public PropertySet remove( String property)
    {
    assert property != null;
    properties_.remove( property, 1);
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

