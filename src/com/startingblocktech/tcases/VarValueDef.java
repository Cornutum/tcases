//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.util.ToString;

import java.util.HashSet;
import java.util.Set;

/**
 * Defines the properties of a value for an {@link IVarDef input variable}.
 *
 * @version $Revision$, $Date$
 */
public class VarValueDef extends Conditional
  {
  /**
   * Defines the type of an input value.
   *
   * @version $Revision$, $Date$
   */
  public enum Type
    {
    VALID( true), FAILURE( false), ONCE( true);

    Type( boolean valid)
      {
      valid_ = valid;
      }

    /**
     * Returns if this type of value is valid member of the variable input domain.
     */
    public boolean isValid()
      {
      return valid_;
      }

    private boolean valid_;
    }

  /**
   * Creates a new VarValueDef object.
   */
  public VarValueDef()
    {
    this( null);
    }

  /**
   * Creates a new VarValueDef object.
   */
  public VarValueDef( String name)
    {
    setName( name);
    setType( Type.VALID);
    setCondition( ICondition.ALWAYS);
    setProperties( null);
    }

  /**
   * Changes the name of this value.
   */
  public void setName( String name)
    {
    name_ = name;
    }

  /**
   * Returns the name of this value.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Changes the type of this value.
   */
  public void setType( Type type)
    {
    type_ = type;
    }

  /**
   * Returns the type of this value.
   */
  public Type getType()
    {
    return type_;
    }

  /**
   * Changes the set of test case properties contributed by this value.
   */
  public void setProperties( Set<String> properties)
    {
    properties_ = new HashSet<String>();
    addProperties( properties);
    }

  /**
   * Adds to the set of test case properties contributed by this value.
   */
  public void addProperties( Set<String> properties)
    {
    if( properties != null)
      {
      getProperties().addAll( properties);
      }
    }

  /**
   * Returns the set of test case properties contributed by this value.
   */
  public Set<String> getProperties()
    {
    return properties_;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .toString();
    }

  private String name_;
  private Type type_;
  private Set<String> properties_;
  }

