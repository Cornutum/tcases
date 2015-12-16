//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Predicate;
import org.apache.commons.collections4.SetUtils;
import org.apache.commons.lang3.ObjectUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

/**
 * Represents a test case for a system function, defining a set of input variable bindings.
 *
 */
public class TestCase extends Annotated implements Comparable<TestCase>
  {
  /**
   * Defines the type of a {@link TestCase}
   *
   */
  public enum Type
    {
    /**
     * Valid input, expected to produce a valid response.
     */
    SUCCESS,

    /**
     * Invalid input, expected to produce an error response.
     */
    FAILURE;
    }

  /**
   * Creates a new TestCase object.
   */
  public TestCase( int id)
    {
    setId( id);
    }

  /**
   * Changes the test case id.
   */
  protected void setId( int id)
    {
    id_ = id;
    }

  /**
   * Returns the test case id.
   */
  public int getId()
    {
    return id_;
    }

  /**
   * Returns the type of this test case.
   */
  public Type getType()
    {
    return
      getInvalidValue() != null
      ? Type.FAILURE
      : Type.SUCCESS;
    }

  /**
   * Adds a new variable binding.
   */
  public TestCase addVarBinding( VarBinding varBinding)
    {
    assert varBinding != null;
    assert varBinding.getVar() != null;

    String varName = varBinding.getVar();
    if( findBinding( varName) >= 0)
      {
      throw new IllegalStateException( "Binding for " + varName + " already defined for testCase=" + getId());
      }
    
    varBindings_.add( varBinding);
    return this;
    }

  /**
   * Removes the binding for the given variable.
   */
  public TestCase removeVarBinding( String name)
    {
    int i = findBinding( name);
    if( i >= 0)
      {
      varBindings_.remove( i);
      }
    
    return this;
    }

  /**
   * Returns the binding for the given variable.
   */
  public VarBinding getVarBinding( String name)
    {
    int i = findBinding( name);
    return
      i >= 0
      ? varBindings_.get( i)
      : null;
    }

  /**
   * Returns the variable that is bound to an invalid value.
   */
  public VarBinding getInvalidValue()
    {
    VarBinding invalidBinding;
    Iterator<VarBinding> bindings;
    for( invalidBinding = null,
           bindings = varBindings_.iterator();

         bindings.hasNext()
           && (invalidBinding = bindings.next()).isValueValid();

         invalidBinding = null);

    return invalidBinding;
    }

  /**
   * Returns the variable bindings for this function.
   */
  public Iterator<VarBinding> getVarBindings()
    {
    return varBindings_.iterator();
    }

  /**
   * Returns the bindings for variables of the given type for this function.
   */
  public Iterator<VarBinding> getVarBindings( final String type)
    {
    return
      IteratorUtils.filteredIterator
      ( varBindings_.iterator(),
        new Predicate<VarBinding>()
        {
        @SuppressWarnings("deprecation")
        public boolean evaluate( VarBinding binding)
          {
          return ObjectUtils.equals( binding.getType(), type);
          }
        });
    }

  /**
   * Returns the index of the binding for the given variable.
   */
  public int findBinding( final String name)
    {
    return
      IterableUtils.indexOf
      ( varBindings_,
        new Predicate<VarBinding>()
        {
        @SuppressWarnings("deprecation")
        public boolean evaluate( VarBinding binding)
          {
          return ObjectUtils.equals( binding.getVar(), name);
          }
        });
    }

  /**
   * Returns the set of {@link IVarDef#getType variable type} identifiers for this test case.
   */
  public String[] getVarTypes()
    {
    TreeSet<String> typeSet = new TreeSet<String>();
    for( Iterator<VarBinding> vars = getVarBindings(); vars.hasNext(); )
      {
      typeSet.add( vars.next().getType());
      }

    String[] types = new String[ typeSet.size()];
    typeSet.toArray( types);
    return types;
    }

  public String toString()
    {
    VarBinding[] bindings = new VarBinding[ varBindings_.size()];
    Arrays.sort( varBindings_.toArray( bindings));
    
    return
      ToString.getBuilder( this)
      .append( getId())
      .append( getType())
      .append( bindings)
      .toString();
    }

  public int compareTo( TestCase other)
    {
    return getId() - other.getId();
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ id_
      ^ new HashSet<VarBinding>(varBindings_).hashCode();
    }

  public boolean equals( Object object)
    {
    TestCase other =
      object != null && object.getClass().equals( getClass())
      ? (TestCase) object
      : null;

    return
      other != null
      && id_ == other.id_
      && SetUtils.isEqualSet( varBindings_, other.varBindings_);
    }

  private int id_;
  private List<VarBinding> varBindings_ = new ArrayList<VarBinding>();
  }

