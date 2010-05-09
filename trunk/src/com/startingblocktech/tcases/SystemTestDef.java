//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;
import static com.startingblocktech.tcases.DefUtils.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

/**
 * Defines the test cases for all functions of a system.
 *
 * @version $Revision$, $Date$
 */
public class SystemTestDef
  {
  /**
   * Creates a new SystemTestDef object.
   */
  public SystemTestDef()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemTestDef object.
   */
  public SystemTestDef( String name)
    {
    setName( name);
    }

  /**
   * Changes the system name.
   */
  public void setName( String name)
    {
    assertIdentifier( name);
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
   * Adds a new function test definition.
   */
  public SystemTestDef addFunctionTestDef( FunctionTestDef functionTestDef)
    {
    assert functionTestDef != null;
    assert functionTestDef.getName() != null;

    if( findFunctionTestDef( functionTestDef.getName()) >= 0)
      {
      throw new IllegalStateException( "Function=" + functionTestDef.getName() + " already defined for system=" + getName());
      }
    
    functionTestDefs_.add( functionTestDef);
    return this;
    }

  /**
   * Removes a function test definition.
   */
  public SystemTestDef removeFunctionTestDef( String name)
    {
    int i = findFunctionTestDef( name);
    if( i >= 0)
      {
      functionTestDefs_.remove(i);
      }

    return this;
    }

  /**
   * Returns the function test definition with the given name.
   */
  public FunctionTestDef getFunctionTestDef( String name)
    {
    int i = findFunctionTestDef( name);
    return i >= 0? functionTestDefs_.get(i) : null;
    }

  /**
   * Returns the function test definitions for this system.
   */
  public Iterator<FunctionTestDef> getFunctionTestDefs()
    {
    return functionTestDefs_.iterator();
    }

  /**
   * Returns the index of the function test definition with the given name.
   */
  protected int findFunctionTestDef( String name)
    {
    int functionCount = name==null? 0 : functionTestDefs_.size();
    int i;
    for( i = 0; i < functionCount && !name.equals( functionTestDefs_.get(i).getName()); i++);
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
  private List<FunctionTestDef> functionTestDefs_ = new ArrayList<FunctionTestDef>();
  }

