//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.apache.commons.collections4.IteratorUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

/**
 * Defines an individual input variable.
 *
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
      IteratorUtils.filteredIterator(
        getValues(),
        VarValueDef::isValid);
    }

  /**
   * Returns an iterator for the set of failure values.
   */
  public Iterator<VarValueDef> getFailureValues()
    {
    return
      IteratorUtils.filteredIterator(
        getValues(),
        valueDef -> !valueDef.isValid());
    }

  /**
   * Adds a value definition for this variable.
   */
  public VarDef addValue( VarValueDef value)
    {
    assert value != null;

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
  public VarValueDef getValue( Object name)
    {
    int i = findValue( name);
    return i >= 0? values_.get(i) : null;
    }

  /**
   * Returns true if the given value can be bound to this variable.
   */
  public boolean isApplicable( VarValueDef value)
    {
    return
      value.isNA()
      ? isOptional()
      : getValue( value.getName()) != null;
    }

  /**
   * Returns the index of the value definition with the given name.
   */
  protected int findValue( Object name)
    {
    int valueCount = values_.size();
    int i;
    for( i = 0; i < valueCount && !Objects.equals( name, values_.get(i).getName()); i++);
    return i < valueCount? i : -1;
    }
  
  private List<VarValueDef> values_ = new ArrayList<VarValueDef>();
  }

