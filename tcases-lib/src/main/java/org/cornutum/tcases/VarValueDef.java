//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.lang3.ObjectUtils;

import java.util.Arrays;
import java.util.Collection;

import static org.cornutum.tcases.Globals.getInputDefValidator;

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
    setProperties( (PropertySet) null);
    }

  /**
   * Changes the name of this value.
   */
  public void setName( String name)
    {
    getInputDefValidator().assertValueName( name);
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
    properties_ = new PropertySet();
    addProperties( properties);
    }

  /**
   * Changes the set of test case properties contributed by this value.
   */
  public void setProperties( PropertySet properties)
    {
    properties_ = new PropertySet();
    addProperties( properties);
    }

  /**
   * Returns the set of test case properties contributed by this value.
   */
  public PropertySet getProperties()
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
      getInputDefValidator().assertPropertyIdentifiers( properties);
      getProperties().addAll( properties);
      }

    return this;
    }

  /**
   * Adds to the set of test case properties contributed by this value.
   */
  public VarValueDef addProperties( PropertySet properties)
    {
    if( properties != null)
      {
      addProperties( IteratorUtils.toList( properties.getProperties()));
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

  @SuppressWarnings("deprecation")
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

  @SuppressWarnings("deprecation")
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ ObjectUtils.hashCode( getName())
      ^ ObjectUtils.hashCode( getType());
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
  private PropertySet properties_;
  }

