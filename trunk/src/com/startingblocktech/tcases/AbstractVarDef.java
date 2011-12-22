//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;
import static com.startingblocktech.tcases.DefUtils.*;

import java.util.Iterator;

/**
 * Base class for {@link IVarDef} implementations.
 *
 * @version $Revision$, $Date$
 */
public abstract class AbstractVarDef extends Conditional implements IVarDef
  {
  /**
   * Creates a new AbstractVarDef object.
   */
  public AbstractVarDef()
    {
    this( null);
    }
  
  /**
   * Creates a new AbstractVarDef object.
   */
  public AbstractVarDef( String name)
    {
    setName( name);
    setType( IVarDef.ARG);
    setCondition( ICondition.ALWAYS);
    }
  
  /**
   * Changes the variable name.
   */
  public void setName( String name)
    {
    assertIdentifier( name);
    name_ = name;
    pathName_ = null;
    }
  
  /**
   * Returns the variable name.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Returns the hierarchical path name of this variable.
   */
  public String getPathName()
    {
    if( pathName_ == null)
      {
      StringBuilder pathName = new StringBuilder();

      IVarDef parent = getParent();
      if( parent != null)
        {
        pathName
          .append( parent.getPathName())
          .append( '.');
        }

      String name = getName();
      if( name != null)
        {
        pathName.append( name);
        }

      pathName_ = pathName.toString();
      }

    return pathName_;
    }

  /**
   * Changes the type identifier for this variable.
   */
  public void setType( String type)
    {
    assertIdentifier( type);
    type_ = type;
    }

  /**
   * Returns the type identifier for this variable.
   */
  public String getType()
    {
    IVarDef parent = getParent();
    return parent==null? type_ : parent.getType();
    }

  /**
   * Changes the parent of this variable.
   */
  public void setParent( IVarDef parent)
    {
    parent_ = parent;
    pathName_ = null;
    }

  /**
   * If this is member of another variable, returns the parent variable. Otherwise, returns null.
   */
  public IVarDef getParent()
    {
    return parent_;
    }

  /**
   * If this variable has member variables, returns an iterator for the member variable list.
   * Otherwise, returns null.
   */
  abstract public Iterator<IVarDef> getMembers();

  /**
   * If this variable defines a value set, returns an iterator for the value set.
   * Otherwise, returns null.
   */
  abstract public Iterator<VarValueDef> getValues();

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getPathName())
      .toString();
    }
  
  private String name_;
  private String type_;
  private IVarDef parent_;
  private String pathName_;
  }

