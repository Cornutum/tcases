//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Defines an individual input variable.
 *
 * @version $Revision$, $Date$
 */
public class VarDef extends AbstractVarDef
  {
  /**
   * Creates a new VarDef object.
   */
  public VarDef()
    {
    this( null);
    }
  
  /**
   * Creates a new VarDef object.
   */
  public VarDef( String name)
    {
    super( name);
    }

  /**
   * Returns true if this variable (has an ancestor that) defines a condition.
   */
  public boolean isOptional()
    {
    IVarDef ancestor;
    boolean optional;
    for( optional = false, ancestor = this;
         !(optional = ancestor.getCondition() != null) && ancestor.getParent() != null;
         ancestor = ancestor.getParent());

    return optional;
    }

  /**
   * If this variable has member variables, returns an iterator for the member variable list.
   * Otherwise, returns null.
   */
  public Iterator<IVarDef> getMembers()
    {
    return null;
    }

  /**
   * If this variable defines a value set, returns an iterator for the value set.
   * Otherwise, returns null.
   */
  public Iterator<VarValueDef> getValues()
    {
    return values_.iterator();
    }
  
  /**
   * Returns the descendant variable with the given name path, relative to this variable.
   */
  public IVarDef find( String[] path)
    {
    return
      path == null || path.length == 0
      ? this
      : null;
    }

  /**
   * Returns an iterator for the set of valid values.
   */
  public Iterator<VarValueDef> getValidValues()
    {
    return
      IteratorUtils.filteredIterator
      ( getValues(),
        new Predicate<VarValueDef>()
          {
          public boolean evaluate( VarValueDef valueDef)
            {
            return valueDef.isValid();
            }
          });
    }

  /**
   * Returns an iterator for the set of failure values.
   */
  public Iterator<VarValueDef> getFailureValues()
    {
    return
      IteratorUtils.filteredIterator
      ( getValues(),
        new Predicate<VarValueDef>()
          {
          public boolean evaluate( VarValueDef valueDef)
            {
            return !valueDef.isValid();
            }
          });
    }

  /**
   * Adds a value definition for this variable.
   */
  public VarDef addValue( VarValueDef value)
    {
    assert value != null;
    assert value.getName() != null;

    if( findValue( value.getName()) >= 0)
      {
      throw new IllegalStateException( "Value=" + value.getName() + " already defined for variable=" + getPathName());
      }

    values_.add( value);
    return this;
    }

  /**
   * Removes a value definition from this variable.
   */
  public VarDef removeValue( String name)
    {
    int i = findValue( name);
    if( i >= 0)
      {
      values_.remove(i);
      }

    return this;
    }

  /**
   * Returns the value definition with the given name.
   */
  public VarValueDef getValue( String name)    {
    int i = findValue( name);
    return i >= 0? values_.get(i) : null;
    }

  /**
   * Returns true if the given value can be bound to this variable.
   */
  public boolean isApplicable( VarValueDef value)
    {
    return
      getValue( value.getName()) != null
      || (VarValueDef.isNA( value) && isOptional());
    }

  /**
   * Returns the index of the value definition with the given name.
   */
  protected int findValue( String name)
    {
    int valueCount = name==null? 0 : values_.size();
    int i;
    for( i = 0; i < valueCount && !name.equals( values_.get(i).getName()); i++);
    return i < valueCount? i : -1;
    }
  
  private List<VarValueDef> values_ = new ArrayList<VarValueDef>();
  }

