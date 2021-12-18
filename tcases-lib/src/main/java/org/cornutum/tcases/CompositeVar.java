//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;

import org.apache.commons.lang3.StringUtils;

import java.util.Iterator;
import java.util.List;

/**
 * Base class for composite variables that are composed of member variables
 */
public abstract class CompositeVar extends AbstractVarDef
  {
  /**
   * Creates a new CompositeVar instance.
   */
  public CompositeVar()
    {
    this( null);
    }
  
  /**
   * Creates a new CompositeVar instance.
   */
  public CompositeVar( String name)
    {
    super( name);
    }

  /**
   * If this variable has member variables, returns an iterator for the member variable list.
   * Otherwise, returns null.
   */
  @Override
  public Iterator<IVarDef> getMembers()
    {
    return getMemberVarDefs().iterator();
    }

  /**
   * If this variable defines a value set, returns an iterator for the value set.
   * Otherwise, returns null.
   */
  @Override
  public Iterator<VarValueDef> getValues()
    {
    return null;
    }

  /**
   * Changes the parent of this variable.
   */
  @Override
  public void setParent( IVarDef parent)
    {
    super.setParent( parent);

    // Reset ancestry for all descendants.
    for( IVarDef member : getMemberVarDefs())
      {
      member.setParent( this);
      }
    }
  
  /**
   * Changes the condition that defines when this element is applicable.
   */
  @Override
  public void setCondition( ICondition condition)
    {
    super.setCondition( condition);

    // Reset ancestry for all descendants.
    for( IVarDef member : getMemberVarDefs())
      {
      member.setParent( this);
      }
    }
  
  /**
   * Returns the descendant variable with the given name path, relative to this variable.
   */
  @Override
  public IVarDef find( String... path)
    {
    return
       path == null || path.length == 0
      ? this
      : getDescendant( path);
    }

  /**
   * Returns the member variable with the given name.
   */
  public IVarDef getMember( String name)
    {
    int i = findMember( name);
    return i >= 0? getMemberVarDefs().get(i) : null;
    }

  /**
   * Returns the descendant variable with the given path, relative to this set.
   */
  public IVarDef getDescendant( String pathName)
    {
    return getDescendant( DefUtils.toPath( pathName));
    }    

  /**
   * Returns the descendant variable with the given path, relative to this set.
   */
  private IVarDef getDescendant( String[] path)
    {
    int pathLength        = path == null? 0 : path.length;
    int parentPathLength  = pathLength - 1;

    int i;
    CompositeVar parent;
    IVarDef descendant;
    for( i = 0,
           parent = this,
           descendant = null;

         i < parentPathLength
           && (descendant = parent.getMember( StringUtils.trimToNull( path[i]))) != null
           && descendant instanceof CompositeVar;

         i++,
           parent = (CompositeVar) descendant);

    return
      i == parentPathLength
      ? parent.getMember( StringUtils.trimToNull( path[i]))
      : null;
    }

  /**
   * Returns the index of the member variable with the given name.
   */
  protected int findMember( String name)
    {
    int memberCount = name==null? 0 : getMemberVarDefs().size();
    int i;
    for( i = 0; i < memberCount && !name.equals( getMemberVarDefs().get(i).getName()); i++);
    return i < memberCount? i : -1;
    }

  /**
   * Returns a list of member variables.
   */
  protected abstract List<IVarDef> getMemberVarDefs();
  }
