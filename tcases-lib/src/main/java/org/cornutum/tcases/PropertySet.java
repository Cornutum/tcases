//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.bag.HashBag;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

/**
 * Defines a set of test case properties.
 *
 */
public class PropertySet implements Iterable<String>
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
   * Creates a new PropertySet object.
   */
  public PropertySet( PropertySet properties)
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
      addAll( propertySet.properties_);
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
      addAll( properties.iterator());
      }

    return this;
    }

  /**
   * Adds a set of properties to this set.
   */
  public PropertySet addAll( Iterator<String> properties)
    {
    if( properties != null)
      {
      while( properties.hasNext())
        {
        add( properties.next());
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
      removeAll( properties.iterator());
      }
    
    return this;
    }

  /**
   * Removes a set of properties from this set.
   */
  public PropertySet removeAll( PropertySet properties)
    {
    if( properties != null)
      {
      removeAll( properties.properties_);
      }
    
    return this;
    }

  /**
   * Removes a set of properties from this set.
   */
  public PropertySet removeAll( Iterator<String> properties)
    {
    if( properties != null)
      {
      while( properties.hasNext())
        {
        remove( properties.next());
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
   * Returns an iterator that visits every instance of each property in this set.
   */
  @Override
public Iterator<String> iterator()
    {
    return properties_.iterator();
    }

  /**
   * Returns an iterator that visits a single instance of each property in this set.
   */
  public Iterator<String> getUniqueProperties()
    {
    return properties_.uniqueSet().iterator();
    }

  /**
   * Returns the number of instances of the given property in this set.
   */
  public int getCount( String property)
    {
    return properties_.getCount( StringUtils.trimToNull( property));
    }

  /**
   * Returns true if this set has no members.
   */
  public boolean isEmpty()
    {
    return properties_.isEmpty();
    }

  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( properties_)
      .toString();
    }

  private HashBag<String> properties_;
  }

