//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.util.ToString;

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

    // Is this variable already bound to a different value?
    if( bindings_.containsKey( var))
      {
      if( !value.equals( bindings_.get( var)))
        {
        throw new VarBoundException( binding, bindings_.get( var));
        }
      }

    // Adding "not applicable" binding?
    else if( value != VarValueDef.NA)
      {
      // No, is this variable applicable to the current test case?
      if( !isVarApplicable( var, properties_))
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
   * Returns true if the given variable is applicable with the given set of test case properties.
   */
  private boolean isVarApplicable( VarDef var, PropertySet properties)
    {
    boolean applicable;
    IVarDef ancestor;
    for( ancestor = var;
         (applicable = Conditional.acquireCondition( ancestor).compatible( properties)) && ancestor.getParent() != null;
         ancestor = ancestor.getParent());
    
    return applicable;
    }

  /**
   * Returns null if all conditions for the given variable are satisfied by the current bindings.
   * Otherwise, returns the unsatisfied conditions
   */
  private ICondition getVarUnsatisfied( VarDef var)
    {
    ICondition unsatisfied;
    IVarDef ancestor;
    for( ancestor = var,
           unsatisfied = null;

         ancestor != null
           && (unsatisfied = Conditional.acquireCondition( ancestor)).satisfied( properties_);
         
         ancestor = ancestor.getParent(),
           unsatisfied = null);
    
    return unsatisfied;
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
           && (isNA( (inapplicable = vars.next()))
               || isVarApplicable( inapplicable, properties));
         
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
   * Returns true if the given value is currently bound to the "not applicable" value.
   */
  public boolean isNA( VarDef var)
    {
    return VarValueDef.isNA( getBinding( var));
    }

  /**
   * Returns true if all conditions for current bindings are satisfied.
   */
  public boolean isComplete()
    {
    boolean complete;
    VarDef incomplete;
    VarValueDef value;
    ICondition unsatisfied;
    Iterator<VarDef> vars;

    // For each variable currently bound...
    for( vars = bindings_.keySet().iterator(),
           complete = true,
           incomplete = null,
           unsatisfied = null,
           value = null;

         vars.hasNext()
           && (complete =

               // Variable not applicable?
               VarValueDef.isNA( (value = getBinding( (incomplete = vars.next()))))
               ||
               ( // Variable conditions satisified?
                 (unsatisfied = getVarUnsatisfied( incomplete)) == null
                 &&
                 // Value condition satisfied?
                 (unsatisfied = Conditional.acquireCondition( value)).satisfied( properties_)));

         );

    if( !complete)
      {
      logger_.debug
        ( "{}: condition unsatisifed, var={}, condition={}",
          new Object[]{ this, incomplete, unsatisfied});
      }

    return complete;
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
          return e1.getKey().compareTo( e2.getKey());
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

  private static final Logger logger_ = LoggerFactory.getLogger( TestCaseDef.class);
  }
