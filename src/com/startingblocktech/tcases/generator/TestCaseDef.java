//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.util.ToString;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Constructs a definition of a {@link TestCase test case}.
 *
 * @version $Revision$, $Date$
 */
public class TestCaseDef
  {

  /**
   * Creates a new TestCaseDef object.
   */
  public TestCaseDef()
    {
    }

  /**
   * Returns the current value binding for the given input variable.
   */
  public VarValueDef getValue( VarDef var)
    {
    return bindings_.get( var);
    }

  /**
   * Adds the variable bindings defined by the given tuple.
   */
  public void addBindings( Tuple tuple) throws BindingException
    {
    for( Iterator<VarBindingDef> bindings = tuple.getBindings();
         bindings.hasNext();
         checkCompatible( bindings.next()));

    for( Iterator<VarBindingDef> bindings = tuple.getBindings();
         bindings.hasNext();
         addBinding( bindings.next()));
    }

  /**
   * Removes the variable bindings defined by the given tuple.
   */
  public void removeBindings( Tuple tuple)
    {
    for( Iterator<VarBindingDef> bindings = tuple.getBindings();
         bindings.hasNext();
         removeBinding( bindings.next()));
    }

  /**
   * Throws an exception if the given variable binding is not
   * compatible with the current test case definition.
   */
  private void checkCompatible( VarBindingDef binding) throws BindingException
    {
    VarDef var = binding.getVarDef();
    VarValueDef value = binding.getValueDef();

    // Is this variable already bound to a different value?
    if( bindings_.containsKey( var))
      {
      if( !value.equals( bindings_.get( var)))
        {
        throw new VarBoundException( binding, bindings_.get( var));
        }
      }

    else
      {
      // Is this variable applicable to the current test case?
      if( !isVarApplicable( var, properties_))
        {
        throw new VarNotApplicableException( binding, properties_);
        }

      // Is this value inconsistent with the current test case?
      if( !value.getCondition().compatible( properties_))
        {
        throw new ValueInconsistentException( binding, properties_);
        }

      // Would this value make a currently bound variable inapplicable?
      VarDef inapplicable = getVarInapplicable( value);
      if( inapplicable != null)
        {
        throw new ValueNotApplicableException( binding, inapplicable);
        }
      }
    }

  /**
   * Returns true if the given variable is applicable with the given set of test case properties.
   */
  private boolean isVarApplicable( VarDef var, PropertySet properties)
    {
    boolean applicable;
    IVarDef ancestor;
    for( ancestor = var;
         (applicable = var.getCondition().compatible( properties)) && ancestor.getParent() != null;
         ancestor = ancestor.getParent());
    
    return applicable;
    }

  /**
   * Returns null if the given value would make not make any currently bound variable inapplicable.
   * Otherwise, return a variable that is inapplicable with this value.
   */
    private VarDef getVarInapplicable( VarValueDef value)
    {
    PropertySet properties = new PropertySet( value.getProperties());
    properties.addAll( properties_);
    
    Iterator<VarDef> vars;
    VarDef inapplicable;
    for( inapplicable = null,
           vars = bindings_.keySet().iterator();

         vars.hasNext()
           && (inapplicable = vars.next()).getCondition().compatible( properties);
         
         inapplicable = null);
    
    return inapplicable;
    }

  /**
   * Adds a new variable binding to the current test case.
   */
  private void addBinding( VarBindingDef binding)
    {
    VarDef var = binding.getVarDef();
    if( !bindings_.containsKey( var))
      {
      VarValueDef value = binding.getValueDef();
      bindings_.put( var, value);
      properties_.addAll( value.getProperties());
      }
    }

  /**
   * Removes a variable binding from the current test case.
   */
  private void removeBinding( VarBindingDef binding)
    {
    VarDef var = binding.getVarDef();
    VarValueDef value = bindings_.remove( var);
    if( value != null)
      {
      properties_.removeAll( value.getProperties());
      }
    }

  /**
   * Returns the value currently bound to the given variable.
   */
  public VarValueDef getBinding( VarDef var)
    {
    return bindings_.get( var);
    }

  /**
   * Create a new test case using the current definition.
   */
  public TestCase createTestCase( int id)
    {
    TestCase testCase = new TestCase( id);
    for( VarDef var : bindings_.keySet())
      {
      testCase.addVarBinding( new VarBinding( var, bindings_.get( var)));
      }

    return testCase;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "bindings", bindings_.size())
      .append( "properties", properties_)
      .toString();
    }

  private Map<VarDef,VarValueDef> bindings_ = new HashMap<VarDef,VarValueDef>();
  private PropertySet properties_ = new PropertySet();
  }
