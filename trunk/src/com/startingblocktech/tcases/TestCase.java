//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

/**
 * Represents a test case for a system function, defining a set of input variable bindings.
 *
 * @version $Revision$, $Date$
 */
public class TestCase implements Comparable<TestCase>
  {
  /**
   * Defines the type of a {@link TestCase}
   *
   * @version $Revision$, $Date$
   */
  public enum Type
    {
    SUCCESS, FAILURE;
    }

  /**
   * Creates a new TestCase object.
   */
  public TestCase( int id)
    {
    setId( id);
    }

  /**
   * Changes the test case id.
   */
  protected void setId( int id)
    {
    id_ = id;
    }

  /**
   * Returns the test case id.
   */
  public int getId()
    {
    return id_;
    }

  /**
   * Returns the type of this test case.
   */
  public Type getType()
    {
    return
      getInvalidValue() != null
      ? Type.FAILURE
      : Type.SUCCESS;
    }

  /**
   * Adds a new variable binding.
   */
  public TestCase addVarBinding( VarBinding varBinding)
    {
    assert varBinding != null;
    assert varBinding.getVar() != null;

    if( findVarBinding( varBinding.getVar()) >= 0)
      {
      throw new IllegalStateException( "Binding for " + varBinding.getVar() + "already defined for testCase=" + getId());
      }
    
    varBindings_.add( varBinding);
    return this;
    }

  /**
   * Removes the binding for the given variable.
   */
  public TestCase removeVarBinding( String name)
    {
    int i = findVarBinding( name);
    if( i >= 0)
      {
      varBindings_.remove(i);
      }

    return this;
    }

  /**
   * Returns the binding for the given variable.
   */
  public VarBinding getVarBinding( String name)
    {
    int i = findVarBinding( name);
    return i >= 0? varBindings_.get(i) : null;
    }

  /**
   * Returns the variable that is bound to an invalid value.
   */
  public VarBinding getInvalidValue()
    {
    int varCount = varBindings_.size();
    int i;
    for( i = 0; i < varCount && varBindings_.get(i).isValueValid(); i++);
    return i < varCount? varBindings_.get(i) : null;
    }

  /**
   * Returns the variable bindings for this function.
   */
  public Iterator<VarBinding> getVarBindings()
    {
    return varBindings_.iterator();
    }

  /**
   * Returns the set of {@link IVarDef#getType variable type} identifiers for this test case.
   */
  public String[] getVarTypes()
    {
    TreeSet<String> typeSet = new TreeSet<String>();
    for( Iterator<VarBinding> vars = getVarBindings(); vars.hasNext(); )
      {
      typeSet.add( vars.next().getType());
      }

    String[] types = new String[ typeSet.size()];
    typeSet.toArray( types);
    return types;
    }

  /**
   * Returns the index of the binding for the given variable.
   */
  protected int findVarBinding( String name)
    {
    int varCount = name==null? 0 : varBindings_.size();
    int i;
    for( i = 0; i < varCount && !name.equals( varBindings_.get(i).getVar()); i++);
    return i < varCount? i : -1;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getId())
      .append( getType())
      .toString();
    }

  public int compareTo( TestCase other)
    {
    return getId() - other.getId();
    }
  
  private int id_;
  private List<VarBinding> varBindings_ = new ArrayList<VarBinding>();
  }

