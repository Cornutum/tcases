//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;

import java.util.Collection;

/**
 * Designates a "not applicable" condition for an {@link IVarDef input variable}.
 *
 */
public class VarNaDef extends VarValueDef
  {
  /**
   * Creates a new VarNaDef object.
   */
  private VarNaDef()
    {
    super( null, Type.VALID);
    }

  /**
   * Changes the name of this value.
   */
  public void setName( String name)
    {
    if( name != null)
      {
      throw new UnsupportedOperationException( "Value name is undefined when variable is not applicable");
      }
    }

  /**
   * Changes the type of this value.
   */
  @Override
public void setType( Type type)
    {
    if( type != Type.VALID)
      {
      throw new UnsupportedOperationException( "Value type is undefined when variable is not applicable");
      }
    }

  /**
   * Returns if this value is a valid member of the variable input domain.
   */
  @Override
public boolean isValid()
    {
    return true;
    }

  /**
   * Adds to the set of test case properties contributed by this value.
   */
  @Override
public VarValueDef addProperties( Collection<String> properties)
    {
    if( properties != null)
      {
      throw new UnsupportedOperationException( "Value properties are undefined when variable is not applicable");
      }

    return this;
    }

  @Override
public boolean equals( Object object)
    {
    return object != null && object.getClass().equals( getClass());
    }

  @Override
public int hashCode()
    {
    return getClass().hashCode();
    }
  
  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .toString();
    }

  /**
   * Returns true if this value indicates a "not applicable" condition for an optional variable.
   */
  @Override
public boolean isNA()
    {
    return true;
    }

  /**
   * The standard "not applicable" value. This value is valid for any variable that is
   * "optional", i.e. that (has an ancestor that) defines a condition.
   */
  public static final VarNaDef NA = new VarNaDef();
  }

