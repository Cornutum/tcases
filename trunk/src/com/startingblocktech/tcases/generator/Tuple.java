//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.PropertySet;
import com.startingblocktech.tcases.VarDef;
import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang.builder.ToStringBuilder;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Represents a set of {@link VarBindingDef input variable bindings}.
 *
 * @version $Revision$, $Date$
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
  public Tuple( VarBindingDef ... bindings)
    {
    setBindings( Arrays.asList( bindings));
    }
  
  /**
   * Creates a new Tuple object.
   */
  public Tuple( Collection<VarBindingDef> bindings)
    {
    setBindings( bindings);
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
   * Returns the number of variable bindings in this tuple.
   */
  public int size()
    {
    return bindings_.size();
    }

  /**
   * Adds a binding to this tuple.
   */
  public Tuple add( VarBindingDef binding)
    {
    remove( binding.getVarDef());

    bindings_.put( binding.getVarDef(), binding);
    properties_.addAll( binding.getValueDef().getProperties());

    return this;
    }

  /**
   * Adds all bindings from the given tuple.
   */
  public Tuple addAll( Tuple tuple)
    {
    for( Iterator<VarBindingDef> bindings = tuple.getBindings(); bindings.hasNext();)
      {
      add( bindings.next());
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
           .getValueDef().getCondition().compatible( properties_));

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
  }

