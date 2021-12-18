//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.ArrayList;
import java.util.List;

/**
 * Defines a named set of input variables.
 *
 */
public class VarSet extends CompositeVar
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

    getMemberVarDefs().add( var);
    var.setParent( this);
    var.setSeqNum( getNextSeqNum());

    return this;
    }

  /**
   * Removes an input variable from this set.
   */
  public VarSet removeMember( String name)
    {
    int i = findMember( name);
    if( i >= 0)
      {
      getMemberVarDefs().remove(i).setParent( null) ;
      }

    return this;
    }

  /**
   * Returns a list of member variables.
   */
  @Override
  protected List<IVarDef> getMemberVarDefs()
    {
    return members_;
    }

  private List<IVarDef> members_ = new ArrayList<IVarDef>();
  }

