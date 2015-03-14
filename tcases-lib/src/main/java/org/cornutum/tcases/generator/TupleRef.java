//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2014, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.util.ToString;
import org.cornutum.tcases.VarBinding;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Represents a reference to a set of input variable bindings.
 *
 */
public class TupleRef
  {
  /**
   * Creates a new TupleRef object.
   */
  public TupleRef()
    {
    }

  /**
   * Adds a new variable binding.
   */
  public TupleRef addVarBinding( VarBinding varBinding)
    {
    assert varBinding != null;
    assert varBinding.getVar() != null;

    String varName = varBinding.getVar();
    if( varBindings_.containsKey( varName))
      {
      throw new IllegalStateException( "Value for " + varName + " already defined");
      }
    
    varBindings_.put( varName, varBinding);
    return this;
    }

  /**
   * Removes the binding for the given variable.
   */
  public TupleRef removeVarBinding( String name)
    {
    varBindings_.remove( name);
    return this;
    }

  /**
   * Returns the binding for the given variable.
   */
  public VarBinding getVarBinding( String name)
    {
    return varBindings_.get( name);
    }

  /**
   * Returns the variable bindings for this tuple.
   */
  public Iterator<VarBinding> getVarBindings()
    {
    return varBindings_.values().iterator();
    }

  /**
   * Returns number of variable bindings for this tuple.
   */
  public int size()
    {
    return varBindings_.size();
    }

  public String toString()
    {
    VarBinding[] bindings = new VarBinding[ varBindings_.size()];
    Arrays.sort( varBindings_.values().toArray( bindings));
    
    return
      ToString.getBuilder( this)
      .append( bindings)
      .toString();
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ varBindings_.hashCode();
    }

  public boolean equals( Object object)
    {
    TupleRef other =
      object != null && object.getClass().equals( getClass())
      ? (TupleRef) object
      : null;

    return
      other != null
      && varBindings_.equals( other.varBindings_);
    }

  private Map<String,VarBinding> varBindings_ = new HashMap<String,VarBinding>();
  }

