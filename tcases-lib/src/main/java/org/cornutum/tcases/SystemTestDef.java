//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.DefUtils.*;

import org.apache.commons.lang3.ObjectUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

/**
 * Defines the test cases for all functions of a system.
 *
 */
public class SystemTestDef extends Annotated
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

  @SuppressWarnings("deprecation")
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ ObjectUtils.hashCode( getName())
      ^ functionTestDefs_.hashCode();
    }

  @SuppressWarnings("deprecation")
  public boolean equals( Object object)
    {
    SystemTestDef other =
      object != null && object.getClass().equals( getClass())
      ? (SystemTestDef) object
      : null;

    return
      other != null
      && ObjectUtils.equals( getName(), other.getName())
      && functionTestDefs_.equals( other.functionTestDefs_);
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

