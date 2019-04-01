//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ObjectUtils;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.DefUtils.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Defines the properties of a value for an {@link IVarDef input variable}.
 *
 */
public class VarValueDef extends Conditional
  {
  /**
   * Defines the type of an input value.
   *
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
  public VarValueDef( Object name)
    {
    this( name, Type.VALID);
    }

  /**
   * Creates a new VarValueDef object.
   */
  public VarValueDef( Object name, Type type)
    {
    setName( name);
    setType( type);
    setProperties( null);
    }

  /**
   * Changes the name of this value.
   */
  public void setName( Object name)
    {
    assertVarValue( name);
    name_ = name;
    }

  /**
   * Returns the name of this value.
   */
  public Object getName()
    {
    return name_;
    }

  /**
   * Returns the external form of the name of this value.
   */
  public Object getExternalName()
    {
    return ObjectUtils.toExternalObject( getName());
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
   * Returns if this value has the given property.
   */
  public boolean hasProperty( String property)
    {
    return properties_.contains( property);
    }

  /**
   * Returns if this value has any properties.
   */
  public boolean hasProperties()
    {
    return !properties_.isEmpty();
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
   * Returns the set of test case properties contributed by this value.
   */
  public Iterable<String> getProperties()
    {
    return properties_;
    }

  /**
   * Adds to the set of test case properties contributed by this value.
   */
  public VarValueDef addProperties( Collection<String> properties)
    {
    if( properties != null)
      {
      assertPropertyIdentifiers( properties);
      properties_.addAll( properties);
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

  public boolean equals( Object object)
    {
    VarValueDef other =
      object != null && object.getClass().equals( getClass())
      ? (VarValueDef) object
      : null;

    return
      other != null
      && Objects.equals( other.getExternalName(), getExternalName())
      && Objects.equals( other.getType(), getType());
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( getExternalName())
      ^ Objects.hashCode( getType());
    }
  
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getName())
      .toString();
    }

  /**
   * Returns true if this value indicates a "not applicable" condition for an optional variable.
   */
  public boolean isNA()
    {
    return false;
    }

  private Object name_;
  private Type type_;
  private Set<String> properties_;
  }

