//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Defines the input space for all functions of a system.
 *
 * @version $Revision$, $Date$
 */
public class SystemInputDef
  {
  /**
   * Creates a new SystemInputDef object.
   */
  public SystemInputDef()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemInputDef object.
   */
  public SystemInputDef( String name)
    {
    setName( name);
    }

  /**
   * Changes the system name.
   */
  public void setName( String name)
    {
    name_ = name;
    }

  /**
   * Returns the system name.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Adds a new function definition.
   */
  public SystemInputDef addFunctionInputDef( FunctionInputDef functionInputDef)
    {
    assert functionInputDef != null;
    assert functionInputDef.getName() != null;

    if( findFunctionInputDef( functionInputDef.getName()) >= 0)
      {
      throw new IllegalStateException( "Function=" + functionInputDef.getName() + "already defined for system=" + getName());
      }
    
    functionInputDefs_.add( functionInputDef);
    return this;
    }

  /**
   * Removes a function definition.
   */
  public SystemInputDef removeFunctionInputDef( String name)
    {
    int i = findFunctionInputDef( name);
    if( i >= 0)
      {
      functionInputDefs_.remove(i);
      }

    return this;
    }

  /**
   * Returns the function definition with the given name.
   */
  public FunctionInputDef getFunctionInputDef( String name)
    {
    int i = findFunctionInputDef( name);
    return i >= 0? functionInputDefs_.get(i) : null;
    }

  /**
   * Returns the function definitions for this system.
   */
  public Iterator<FunctionInputDef> getFunctionInputDefs()
    {
    return functionInputDefs_.iterator();
    }

  /**
   * Returns the index of the function definition with the given name.
   */
  protected int findFunctionInputDef( String name)
    {
    int functionCount = name==null? 0 : functionInputDefs_.size();
    int i;
    for( i = 0; i < functionCount && !name.equals( functionInputDefs_.get(i).getName()); i++);
    return i < functionCount? i : -1;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .toString();
    }

  private String name_;
  private List<FunctionInputDef> functionInputDefs_ = new ArrayList<FunctionInputDef>();
  }

