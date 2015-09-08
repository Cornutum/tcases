//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.Predicate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Constructs a definition of a {@link TestCase test case}.
 *
 */
public class TestCaseDef implements Comparable<TestCaseDef>
  {  
  /**
   * A Predicate that returns true for a binding that is compatible with this test case.
   *
   */
  private class BindingCompatible implements Predicate<VarBindingDef>
    {
    public boolean evaluate( VarBindingDef binding)
      {
      boolean compatible = true;
      try
        {
        checkCompatible( binding);
        }
      catch( BindingException e)
        {
        compatible = false;
        }

      return compatible;
      }
    }

  /**
   * Creates a new TestCaseDef object.
   */
  public TestCaseDef()
    {
    }

  /**
   * Creates a new TestCaseDef object.
   */
  public TestCaseDef( TestCaseDef other)
    {
    this();
    if( other != null)
      {
      for( VarDef var : other.bindings_.keySet())
        {
        addBinding( var, other.getBinding( var));
        }
      }
    }

  /**
   * Changes the (optional) id for this test case.
   */
  public void setId( Integer id)
    {
    id_ = id;
    }

  /**
   * Returns the (optional) id for this test case.
   */
  public Integer getId()
    {
    return id_;
    }

  /**
   * Returns the current value binding for the given input variable.
   */
  public VarValueDef getValue( VarDef var)
    {
    return bindings_.get( var);
    }

  /**
   * Returns true if this test case uses all the bindings in the given tuple.
   */
  public boolean usesTuple( Tuple tuple)
    {
    Iterator<VarBindingDef> tupleBindings;
    boolean uses;
    for( tupleBindings = tuple.getBindings(),
           uses = tuple.size() > 0;

         uses && tupleBindings.hasNext();

         uses = usesBinding( tupleBindings.next()));

    return uses;
    }

  /**
   * Returns true if this test case uses the given binding.
   */
  public boolean usesBinding( VarBindingDef binding)
    {
    return binding.getValueDef().equals( getValue( binding.getVarDef()));
    }

  /**
   * If the given tuple is compatible with the current test case definition, adds any
   * new bindings. Returns null if the tuple is incompatible. Otherwise, returns a new
   * tuple containing the new bindings actually added.
   */
  public Tuple addCompatible( Tuple tuple)
    {
    Tuple newBindings = null;

    try
      {
      newBindings = addBindings( tuple);
      logger_.trace( "Adding tuple={}, testCase={}", tuple, this);

      }
    catch( BindingException be)
      {
      logger_.trace
        ( "Can't add tuple={}: {}, testCase={}",
          new Object[]{ tuple, be.getMessage(), this});
      }

    return newBindings;
    }

  /**
   * Returns if the given tuple is compatible with the current test case definition.
   */
  public boolean isCompatible( Tuple tuple)
    {
    boolean compatible = true;

    try
      {
      checkCompatible( tuple);
      }
    catch( BindingException be)
      {
      compatible = false;
      }

    return compatible;
    }

  /**
   * Adds the variable bindings defined by the given tuple.
   * Returns a new tuple containing the new bindings actually added.
   */
  public Tuple addBindings( Tuple tuple) throws BindingException
    {
    checkCompatible( tuple);

    Tuple newBindings = new Tuple();
    for( Iterator<VarBindingDef> bindings = tuple.getBindings();
         bindings.hasNext();
         )
      {
      VarBindingDef binding = bindings.next();
      if( addBinding( binding))
        {
        newBindings.add( binding);
        }
      }

    return newBindings;
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
   * Throws an exception if the tuple is not
   * compatible with the current test case definition.
   */
  private void checkCompatible( Tuple tuple) throws BindingException
    {
    for( Iterator<VarBindingDef> bindings = tuple.getBindings();
         bindings.hasNext();
         checkCompatible( bindings.next()));
    }

  /**
   * Throws an exception if the given variable binding is not
   * compatible with the current test case definition.
   */
  private void checkCompatible( VarBindingDef binding) throws BindingException
    {
    VarDef var = binding.getVarDef();
    VarValueDef value = binding.getValueDef();
    VarValueDef prevValue = bindings_.get( var);

    // Is this variable already bound to a different valid value?
    if( prevValue != null)
      {
      if( !value.equals( prevValue))
        {
        throw new VarBoundException( binding, prevValue);
        }
      }

    // Adding "not applicable" binding?
    else if( value != VarValueDef.NA)
      {
      // No, is this variable applicable to the current test case?
      if( !var.getEffectiveCondition().compatible( properties_))
        {
        throw new VarNotApplicableException( binding, properties_);
        }

      // Is this value inconsistent with the current test case?
      if( !Conditional.acquireCondition( value).compatible( properties_))
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
   * Returns null if the given value would not make any currently bound variable inapplicable.
   * Otherwise, return a variable that is inapplicable with this value.
   */
  private VarDef getVarInapplicable( VarValueDef value)
    {
    PropertySet properties =
      new PropertySet()
      .addAll( value.getProperties())
      .addAll( properties_);
    
    Iterator<VarDef> vars;
    VarDef inapplicable;
    for( inapplicable = null,
           vars = getVars();

         vars.hasNext()
           && (isNA( (inapplicable = vars.next()))
               || inapplicable.getEffectiveCondition().compatible( properties));
         
         inapplicable = null);
    
    return inapplicable;
    }

  /**
   * Adds a new variable binding to the current test case, if necessary.
   * Returns true if a new binding was actually added.
   */
  private boolean addBinding( VarBindingDef binding)
    {
    VarDef var = binding.getVarDef();
    boolean added = !bindings_.containsKey( var);
    if( added)
      {
      addBinding( var, binding.getValueDef());
      }

    return added;
    }

  /**
   * Adds a new variable binding to the current test case.
   */
  private void addBinding( VarDef var, VarValueDef value)
    {
    bindings_.put( var, value);
    properties_.addAll( value.getProperties());
    required_ = null;
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
      required_ = null;
      }
    }

  /**
   * Returns the variables currently bound in this current test case.
   */
  public Iterator<VarDef> getVars()
    {
    return bindings_.keySet().iterator();
    }

  /**
   * Returns the value currently bound to the given variable.
   */
  public VarValueDef getBinding( VarDef var)
    {
    return bindings_.get( var);
    }

  /**
   * Returns true if the given value is currently bound to the "not applicable" value.
   */
  public boolean isNA( VarDef var)
    {
    return VarValueDef.isNA( getBinding( var));
    }

  /**
   * Returns the variable bound to an invalid value, if any.
   */
  public VarDef getInvalidVar()
    {
    VarDef invalidVar;
    Iterator<VarDef> vars;
    for( vars = getVars(),
           invalidVar = null;

         vars.hasNext()
           && getValue( (invalidVar = vars.next())).isValid();

         invalidVar = null);
    
    return invalidVar;
    }

  /**
   * Returns the conditions of current bindings not yet satisfied.
   */
  public IConjunct getRequired()
    {
    if( required_ == null)
      {
      AllOf conditions = new AllOf();
      for( Iterator<VarDef> vars = getVars();
           vars.hasNext();)
        {
        VarDef var = vars.next();
        VarValueDef value = getBinding( var);
        if( !VarValueDef.isNA( value))
          {
          conditions.add( var.getEffectiveCondition());
          conditions.add( Conditional.acquireCondition( value));
          }
        }

      required_ = Cnf.getUnsatisfied( Cnf.convert( conditions), properties_);
      }

    return required_;
    }

  /**
   * Returns the Predicate that returns true for a binding that is compatible with this test case.
   */
  public Predicate<VarBindingDef> getBindingCompatible()
    {
    if( bindingCompatible_ == null)
      {
      bindingCompatible_ = new BindingCompatible();
      }

    return bindingCompatible_;
    }

  /**
   * Returns true if all conditions for current bindings are satisfied.
   */
  public boolean isSatisfied()
    {
    return getRequired().getDisjunctCount() == 0;
    }

  /**
   * Returns true if the current {@link #getRequired required condition} for this test case
   * is unsatisfiable.
   */
  public boolean isInfeasible()
    {
    IAssertion unsatisfiable; 
    Iterator<IDisjunct> disjuncts;
    for( unsatisfiable = null,
           disjuncts = getRequired().getDisjuncts();

         unsatisfiable == null
           && disjuncts.hasNext();)
      {
      for( Iterator<IAssertion> assertions = disjuncts.next().getAssertions();

           assertions.hasNext()
             && !(unsatisfiable = assertions.next()).getClass().equals( AssertNot.class);

           unsatisfiable = null);
      }

    if( unsatisfiable != null)
        {
        logger_.trace( "Infeasible, can no longer satisfy {}, testCase={}", unsatisfiable, this);
        }
    
    return unsatisfiable != null;
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

  public int compareTo( TestCaseDef other)
    {
    int id = getId()==null? Integer.MAX_VALUE : getId();
    int otherId  = other.getId()==null? Integer.MAX_VALUE : other.getId();
    return id - otherId;
    }

  public String toString()
    {
    ArrayList<VarDef> vars = new ArrayList<VarDef>( bindings_.keySet());
    Collections.sort
      ( vars,
        new Comparator<VarDef>()
        {
        public int compare( VarDef v1, VarDef v2)
          {
          return v1.getPosition().compareTo( v2.getPosition());
          }
        });
    StringBuilder bindings = new StringBuilder( "[");
    for( VarDef var : vars)
      {
      if( bindings.length() > 1)
        {
        bindings.append( ", ");
        }
      bindings
        .append( var.getName())
        .append( '=')
        .append( bindings_.get( var).getName());
      }
    bindings.append( ']');
        
    return
      ToString.getBuilder( this)
      .append( "id", getId())
      .append( "bindings", bindings.toString())
      .append( "properties", properties_)
      .toString();
    }

  private Integer id_;
  private Map<VarDef,VarValueDef> bindings_ = new HashMap<VarDef,VarValueDef>();
  private PropertySet properties_ = new PropertySet();
  private IConjunct required_;
  private Predicate<VarBindingDef> bindingCompatible_;

  private static final Logger logger_ = LoggerFactory.getLogger( TestCaseDef.class);
  }
