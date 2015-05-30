//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.DefUtils.*;

import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

/**
 * Defines the input space for a specific function.
 *
 */
public class FunctionInputDef extends Annotated
  {
  /**
   * Creates a new FunctionInputDef object.
   */
  public FunctionInputDef()
    {
    this( null);
    }
  
  /**
   * Creates a new FunctionInputDef object.
   */
  public FunctionInputDef( String name)
    {
    setName( name);
    }

  /**
   * Changes the function name.
   */
  public void setName( String name)
    {
    if( name != null)
      {
      assertIdentifier( name);
      }
    name_ = name;
    }

  /**
   * Returns the function name.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Adds a new variable definition.
   */
  public FunctionInputDef addVarDef( IVarDef varDef)
    {
    assert varDef != null;
    assert varDef.getName() != null;

    if( findVarDef( varDef.getName()) >= 0)
      {
      throw new IllegalStateException( "Variable=" + varDef.getName() + " already defined for function=" + getName());
      }
    
    vars_.add( varDef);
    return this;
    }

  /**
   * Removes a variable definition.
   */
  public FunctionInputDef removeVarDef( String name)
    {
    int i = findVarDef( name);
    if( i >= 0)
      {
      vars_.remove(i);
      }

    return this;
    }

  /**
   * Returns the variable definition with the given name.
   */
  public IVarDef getVarDef( String name)
    {
    int i = findVarDef( name);
    return i >= 0? vars_.get(i) : null;
    }

  /**
   * Returns the variable definitions for this function.
   */
  public Iterator<IVarDef> getVarDefs()
    {
    return vars_.iterator();
    }

  /**
   * Returns the set of {@link IVarDef#getType variable type} identifiers for this function.
   */
  public String[] getVarTypes()
    {
    TreeSet<String> typeSet = new TreeSet<String>();
    for( Iterator<IVarDef> vars = getVarDefs(); vars.hasNext(); )
      {
      typeSet.add( vars.next().getType());
      }

    String[] types = new String[ typeSet.size()];
    typeSet.toArray( types);
    return types;
    }

  /**
   * Returns the index of the variable definition with the given name.
   */
  protected int findVarDef( String name)
    {
    int varCount = name==null? 0 : vars_.size();
    int i;
    for( i = 0; i < varCount && !name.equals( vars_.get(i).getName()); i++);
    return i < varCount? i : -1;
    }

  /**
   * Returns the variable definition with the given path name.
   */
  public IVarDef findVarPath( String pathName)
    {
    String[] path = DefUtils.toPath( pathName);
    IVarDef  var;

    return
      path == null?
      null :

      (var = getVarDef( StringUtils.trimToNull( path[0]))) == null?
      null :

      var.find( Arrays.copyOfRange( path, 1, path.length));
    }

  /**
   * Returns the individual variable definition with the given path name.
   */
  public VarDef findVarDefPath( String pathName)
    {
    IVarDef var = findVarPath( pathName);
    return
      var != null && var.getClass().equals( VarDef.class)
      ? (VarDef) var
      : null;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .toString();
    }

  private String name_;
  private List<IVarDef> vars_ = new ArrayList<IVarDef>();
  }

