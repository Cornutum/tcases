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

import org.apache.commons.collections15.Predicate;

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
 * @version $Revision$, $Date$
 */
public class TestCaseDef implements Comparable<TestCaseDef>
  {
  /**
   * A Predicate that returns true for a variable that can partially satisfy the
   * current {@link #getRequired required condition} for this test case.
   *
   * @version $Revision$, $Date$
   */
  private class VarSatisfies implements Predicate<VarDef>
    {
    public boolean evaluate( VarDef var)
      {
      boolean satisfies;
      Iterator<VarValueDef> values;
      IConjunct required;

      for( values = var.getValues(),
             required = getRequired(),
             satisfies = false;

           !satisfies
             && values.hasNext();

           satisfies = Cnf.satisfiesSome( required, values.next().getProperties()));
      
      return satisfies;
      }
    }
  
  /**
   * A Predicate that returns true for a tuple that can partially satisfy the
   * current {@link #getRequired required condition} for this test case.
   *
   * @version $Revision$, $Date$
   */
  private class TupleSatisfies implements Predicate<Tuple>
    {
    public boolean evaluate( Tuple tuple)
      {
      return Cnf.satisfiesSome( getRequired(), tuple.getProperties());
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
      logger_.debug( "{}: adding tuple={}", this, tuple);

      }
    catch( BindingException be)
      {
      logger_.debug
        ( "{}, can't add tuple={}: {}",
          new Object[]{ this, tuple, be.getMessage()});
      }

    return newBindings;
    }

  /**
   * Adds the variable bindings defined by the given tuple.
   * Returns a new tuple containing the new bindings actually added.
   */
  public Tuple addBindings( Tuple tuple) throws BindingException
    {
    for( Iterator<VarBindingDef> bindings = tuple.getBindings();
         bindings.hasNext();
         checkCompatible( bindings.next()));

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
      if( prevValue.isValid() && !value.equals( prevValue))
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
   * Returns null if the given value would make not make any currently bound variable inapplicable.
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
   * Returns the Predicate that returns true for a variable that partially satisfies the
   * current {@link #getRequired required condition} for this test case.
   */
  public Predicate<VarDef> getVarSatisfies()
    {
    if( varSatisfies_ == null)
      {
      varSatisfies_ = new VarSatisfies();
      }

    return varSatisfies_;
    }

  /**
   * Returns the Predicate that returns true for a tuple that partially satisfies the
   * current {@link #getRequired required condition} for this test case.
   */
  public Predicate<Tuple> getTupleSatisfies()
    {
    if( tupleSatisfies_ == null)
      {
      tupleSatisfies_ = new TupleSatisfies();
      }

    return tupleSatisfies_;
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
    boolean feasible;
    Iterator<IDisjunct> disjuncts;
    for( feasible = true,
           disjuncts = getRequired().getDisjuncts();

         feasible
           && disjuncts.hasNext();)
      {
      for( Iterator<IAssertion> assertions = disjuncts.next().getAssertions();
           feasible && assertions.hasNext();
           feasible = !assertions.next().getClass().equals( AssertNot.class));
      }
    
    return !feasible;
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
    ArrayList<Map.Entry<VarDef,VarValueDef>> bindings = new ArrayList<Map.Entry<VarDef,VarValueDef>>( bindings_.entrySet());
    Collections.sort
      ( bindings,
        new Comparator<Map.Entry<VarDef,VarValueDef>>()
        {
        public int compare( Map.Entry<VarDef,VarValueDef> e1, Map.Entry<VarDef,VarValueDef> e2)
          {
          IVarDef.Position pos1 = e1.getKey().getPosition();
          IVarDef.Position pos2 = e2.getKey().getPosition();
          return pos1.compareTo( pos2);
          }
        }); 
    return
      ToString.getBuilder( this)
      .append( "id", getId())
      .append( "bindings", bindings)
      .append( "properties", properties_)
      .toString();
    }

  private Integer id_;
  private Map<VarDef,VarValueDef> bindings_ = new HashMap<VarDef,VarValueDef>();
  private PropertySet properties_ = new PropertySet();
  private IConjunct required_;
  private Predicate<VarDef> varSatisfies_;
  private Predicate<Tuple> tupleSatisfies_;

  private static final Logger logger_ = LoggerFactory.getLogger( TestCaseDef.class);
  }
