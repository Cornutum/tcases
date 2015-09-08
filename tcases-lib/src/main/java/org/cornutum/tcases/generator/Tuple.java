//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.PropertySet;
import org.cornutum.tcases.VarBindingDef;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.VarValueDef;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Represents a set of {@link VarBindingDef input variable bindings}.
 *
 */
public class Tuple
  {
  /**
   * Creates a new Tuple object.
   */
  public Tuple()
    {
    this( (Collection<VarBindingDef>) null);
    }
  
  /**
   * Creates a new Tuple object.
   */
  public Tuple( Tuple other)
    {
    this();
    addAll( other);
    setOnce( other.isOnce());
    }
  
  /**
   * Creates a new Tuple object.
   */
  public Tuple( VarBindingDef ... bindings)
    {
    this( Arrays.asList( bindings));
    }
  
  /**
   * Creates a new Tuple object.
   */
  public Tuple( Collection<VarBindingDef> bindings)
    {
    setBindings( bindings);
    }
  
  /**
   * Returns null if all of the given bindings cannot be included in compatible Tuple.
   * Otherwise, returns a new compatible Tuple containing all of the given bindings.
   */
  public static Tuple of( Collection<VarBindingDef> tupleBindings)
    {
    Tuple tuple = new Tuple();
    
    boolean bindingsCompatible;
    Iterator<VarBindingDef> bindings;
    VarBindingDef nextBinding;
    for( bindings = tupleBindings.iterator(),
           bindingsCompatible = true; 

         bindings.hasNext()
           && (bindingsCompatible = isBindingCompatible( tuple, (nextBinding = bindings.next())));

         tuple.add( nextBinding));
    
    return
      bindingsCompatible && tuple.isCompatible()
      ? tuple
      : null;
    }

  /**
   * Returns true if the given binding can be added to the give tuple.
   */
  private static boolean isBindingCompatible( Tuple tuple, VarBindingDef binding)
    {
    VarValueDef currentValue = tuple.getBinding( binding.getVarDef());
    return (currentValue == null || currentValue.equals( binding.getValueDef()));
    }

  /**
   * Changes the variable bindings for this tuple.
   */
  public void setBindings( Collection<VarBindingDef> bindings)
    {
    bindings_ = new HashMap<VarDef,VarBindingDef>();
    properties_ = new PropertySet();
    
    if( bindings != null)
      {
      for( VarBindingDef binding : bindings)
        {
        add( binding);
        }
      }
    }

  /**
   * Returns the variable bindings for this tuple.
   */
  public Iterator< VarBindingDef> getBindings()
    {
    return bindings_.values().iterator();
    }

  /**
   * Returns true if this tuple contains the given binding.
   */
  public boolean contains( VarBindingDef binding)
    {
    return binding.equals( bindings_.get( binding.getVarDef()));
    }

  /**
   * Returns the value bound by this tuple for the given variable.
   */
  public VarValueDef getBinding( VarDef var)
    {
    VarBindingDef binding = bindings_.get( var);
    return
      binding == null
      ? null
      : binding.getValueDef();
    }

  /**
   * Returns the number of variable bindings in this tuple.
   */
  public int size()
    {
    return bindings_.size();
    }

  /**
   * Returns the properties of the variable bindings for this tuple.
   */
  public PropertySet getProperties()
    {
    return properties_;
    }

  /**
   * Adds a binding to this tuple.
   */
  public Tuple add( VarBindingDef binding)
    {
    if( binding != null)
      {
      VarDef var = binding.getVarDef();
      if( var == null)
        {
        throw new IllegalArgumentException( "Invalid binding=" + binding + ": variable undefined");
        }

      VarValueDef value = binding.getValueDef();
      if( value == null)
        {
        throw new IllegalArgumentException( "Invalid binding=" + binding + ": value undefined");
        }
      
      remove( var);

      bindings_.put( var, binding);
      properties_.addAll( value.getProperties());
      }

    return this;
    }

  /**
   * Adds all bindings from the given tuple.
   */
  public Tuple addAll( Tuple tuple)
    {
    if( tuple != null)
      {
      for( Iterator<VarBindingDef> bindings = tuple.getBindings(); bindings.hasNext();)
        {
        add( bindings.next());
        }
      }

    return this;
    }

  /**
   * Removes a binding from this tuple.
   */
  public Tuple remove( VarDef var)
    {
    VarBindingDef binding = bindings_.remove( var);
    if( binding != null)
      {
      properties_.removeAll( binding.getValueDef().getProperties());
      }

    return this;
    }

  /**
   * Changes if this tuple should be used in at most one test case.
   */
  public void setOnce( boolean once)
    {
    once_ = once;
    }

  /**
   * Returns if this tuple should be used in at most one test case.
   */
  public boolean isOnce()
    {
    return once_;
    }

  /**
   * Returns true if this tuple contains compatible variable bindings.
   */
  public boolean isCompatible()
    {
    boolean compatible;
    Iterator<VarBindingDef> bindings;
    VarBindingDef binding = null;

    for( compatible = true,
           bindings = getBindings();

         compatible
           && bindings.hasNext();

         compatible =
           (binding = bindings.next())
           .getEffectiveCondition()
           .compatible( properties_));

    if( !compatible && size() == 1)
      {
      throw
        new IllegalStateException
        ( "Invalid " + binding
          + ", value condition=" + binding.getValueDef().getCondition()
          + " is incompatible its own properties=" + binding.getValueDef().getProperties());
      }
    
    return compatible;
    }

  @SuppressWarnings("deprecation")
  public boolean equals( Object object)
    {
    Tuple other =
      object != null && object.getClass().equals( getClass())
      ? (Tuple) object
      : null;

    return
      other != null
      && ObjectUtils.equals( other.bindings_, bindings_);
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ (bindings_.hashCode());
    }

  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);
    if( !bindings_.isEmpty())
      {
      builder.append( bindings_.values());
      }
    return builder.toString();
    }

  private Map<VarDef,VarBindingDef> bindings_;
  private PropertySet properties_;
  private boolean once_;
  }
