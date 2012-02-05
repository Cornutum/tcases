//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import com.startingblocktech.tcases.conditions.ICondition;
import com.startingblocktech.tcases.util.ToString;
import static com.startingblocktech.tcases.DefUtils.*;

import org.apache.commons.lang.ObjectUtils;

import java.util.Arrays;
import java.util.Collection;
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
    /**
     * A valid input value
     */
    VALID( true),

    /**
     * An invalid input value
     */
    FAILURE( false),

    /**
     * A valid input value that need not appear in multiple test case combinations.
     */
    ONCE( true);

    Type( boolean valid)
      {
      valid_ = valid;
      }

    /**
     * Returns if this type of value is a valid member of the variable input domain.
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
    this( name, Type.VALID);
    }

  /**
   * Creates a new VarValueDef object.
   */
  public VarValueDef( String name, Type type)
    {
    setName( name);
    setType( type);
    setCondition( ICondition.ALWAYS);
    setProperties( null);
    }

  /**
   * Changes the name of this value.
   */
  public void setName( String name)
    {
    assertIdentifier( name);
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
   * Returns if this value is a valid member of the variable input domain.
   */
  public boolean isValid()
    {
    return getType().isValid();
    }

  /**
   * Changes the set of test case properties contributed by this value.
   */
  public void setProperties( Collection<String> properties)
    {
    properties_ = new HashSet<String>();
    addProperties( properties);
    }

  /**
   * Adds to the set of test case properties contributed by this value.
   */
  public VarValueDef addProperties( Collection<String> properties)
    {
    if( properties != null)
      {
      assertPropertyIdentifiers( properties);
      getProperties().addAll( properties);
      }

    return this;
    }

  /**
   * Adds to the set of test case properties contributed by this value.
   */
  public VarValueDef addProperties( String ... properties)
    {
    return addProperties( Arrays.asList( properties));
    }

  /**
   * Returns the set of test case properties contributed by this value.
   */
  public Set<String> getProperties()
    {
    return properties_;
    }

  public boolean equals( Object object)
    {
    VarValueDef other =
      object != null && object.getClass().equals( getClass())
      ? (VarValueDef) object
      : null;

    return
      other != null
      && ObjectUtils.equals( other.getName(), getName())
      && ObjectUtils.equals( other.getType(), getType());
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ (getName()==null? 0 : getName().hashCode())
      ^ (getType()==null? 0 : getType().hashCode());
    }
  
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .toString();
    }

  /**
   * Returns true if the given value is the standard {@link #NA "not applicable"} value.
   */
  public static boolean isNA( VarValueDef value)
    {
    return value == NA;
    }

  /**
   * The standard "not applicable" value. This value is valid for any variable that is
   * "optional", i.e. that (has an ancestor that) defines a condition.
   */
  public static final VarValueDef NA = new VarValueDef( "NA");

  private String name_;
  private Type type_;
  private Set<String> properties_;
  }

