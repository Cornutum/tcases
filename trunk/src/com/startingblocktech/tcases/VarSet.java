//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Defines a named set of input variables.
 *
 * @version $Revision$, $Date$
 */
public class VarSet extends AbstractVarDef
  {
  /**
   * Creates a new VarSet object.
   */
  public VarSet()
    {
    this( null);
    }
  
  /**
   * Creates a new VarSet object.
   */
  public VarSet( String name)
    {
    super( name);
    }

  /**
   * If this variable has member variables, returns an iterator for the member variable list.
   * Otherwise, returns null.
   */
  public Iterator<IVarDef> getMembers()
    {
    return members_.iterator();
    }

  /**
   * If this variable defines a value set, returns an iterator for the value set.
   * Otherwise, returns null.
   */
  public Iterator<VarValueDef> getValues()
    {
    return null;
    }

  /**
   * Adds an input variable to this set.
   */
  public VarSet addMember( IVarDef var)
    {
    assert var != null;
    assert var.getName() != null;

    if( findMember( var.getName()) >= 0)
      {
      throw new IllegalStateException( "Member=" + var.getName() + " already defined for varSet=" + getPathName());
      }

    members_.add( var);
    var.setParent( this);

    return this;
    }

  /**
   * Removes an input variable to this set.
   */
  public VarSet removeMember( String name)
    {
    int i = findMember( name);
    if( i >= 0)
      {
      members_.remove(i).setParent( null) ;
      }

    return this;
    }

  /**
   * Returns the member variable with the given name.
   */
  public IVarDef getMember( String name)
    {
    int i = findMember( name);
    return i >= 0? members_.get(i) : null;
    }

  /**
   * Returns the descendant variable with the given path, relative to this set.
   */
  public IVarDef getDescendant( String pathName)
    {
    String[]  path              = pathName == null? null : pathName.split( "\\.");
    int       pathLength        = path == null? 0 : path.length;
    int       parentPathLength  = pathLength - 1;

    int       i;
    VarSet    parent;
    IVarDef   descendant;
    for( i = 0,
           parent = this,
           descendant = null;

         i < parentPathLength
           && (descendant = parent.getMember( StringUtils.trimToNull( path[i]))) != null
           && descendant.getClass().equals( getClass());

         i++,
           parent = (VarSet) descendant);

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
    int memberCount = name==null? 0 : members_.size();
    int i;
    for( i = 0; i < memberCount && !name.equals( members_.get(i).getName()); i++);
    return i < memberCount? i : -1;
    }

  private List<IVarDef> members_ = new ArrayList<IVarDef>();
  }

