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
import static org.cornutum.tcases.util.CollectionUtils.toCsv;
import static org.cornutum.tcases.util.CollectionUtils.toOrderedSet;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * Supplies a definition of a {@link TestCase test case}.
 */
public class TestCaseDef implements ITestCaseDef
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
        addBinding( var, other.getValue( var));
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
  @Override 
  public Integer getId()
    {
    return id_;
    }

  /**
   * Changes the (optional) name for this test case.
   */
  public void setName( String name)
    {
    name_ = name;
    }

  /**
   * Changes the name for this test case to identify the given tuple.
   */
  public void setName( Tuple tuple)
    {
    setName(
      toStream( tuple.getBindings())
      .map( VarBindingDef::getVarDef)
      .sorted( Comparator.comparing( IVarDef::getPosition))
      .map( varDef -> String.format( "%s=%s", varDef.getPathName(), toCsv( Stream.of( tuple.getBinding( varDef).getName()))))
      .collect( joining( "&")));
    }

  /**
   * Returns the (optional) name for this test case.
   */
  @Override
  public String getName()
    {
    return name_;
    }

  /**
   * Returns the current value binding for the given input variable.
   */
  @Override
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
      newBindings =
        Optional.of( addBindings( tuple))
        .filter( added -> added.size() > 0)
        .orElse( null);
      
      if( newBindings != null)
        {
        logger_.trace( "Adding tuple={}, testCase={}", tuple, this);
        }

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
    boolean compatible;

    try
      {
      checkCompatible( tuple);
      compatible = true;
      }
    catch( BindingException be)
      {
      compatible = false;
      }

    return compatible;
    }

  /**
   * Returns if the given binding is compatible with the current test case definition.
   */
  public boolean isCompatible( VarBindingDef binding)
    {
    boolean compatible;

    try
      {
      checkCompatible( binding);
      compatible = true;
      }
    catch( BindingException e)
      {
      compatible = false;
      }

    return compatible;
    }

  /**
   * Returns if the given variable is applicable to the current test case definition.
   */
  public boolean isApplicable( VarDef var)
    {
    return var.getEffectiveCondition().satisfied( properties_);
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
   * Removes the most-recently added variable bindings until the number of bindings reverts to
   * the given previous count.
   */
  public void revertBindings( int prevCount)
    {
    toStream( getVars()).collect( toList())
      .subList( prevCount, getBindingCount()).stream()
      .forEach( var -> removeBinding( var));
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
    else if( !value.isNA())
      {
      // No, is this variable inconsistent with the current test case?
      if( !var.getEffectiveCondition().compatible( properties_))
        {
        throw new VarNotApplicableException( binding, properties_);
        }

      // Is this value inconsistent with the current test case?
      boolean valueCompatible = Optional.ofNullable( value.getCondition()).map( c -> c.compatible( properties_)).orElse( true);
      if( !valueCompatible)
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
      .addAll( value.getProperties().iterator())
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
   * Adds a new variable binding to this test case, if necessary.
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
   * Adds a new variable binding to this test case.
   */
  private void addBinding( VarDef var, VarValueDef value)
    {
    bindings_.put( var, value);
    properties_.addAll( value.getProperties().iterator());
    required_ = null;
    }

  /**
   * Removes a variable binding from this test case.
   */
  private void removeBinding( VarBindingDef binding)
    {
    removeBinding( binding.getVarDef());
    }

  /**
   * Removes a variable binding from this test case.
   */
  private void removeBinding( VarDef var)
    {
    VarValueDef value = bindings_.remove( var);
    logger_.trace( "Removing binding for {}={}, testCase={}", var.getName(), value.isNA()? "N/A" : value.getName(), this);

    if( value != null)
      {
      properties_.removeAll( value.getProperties().iterator());
      required_ = null;
      }
    }

  /**
   * Returns the variables currently bound in this test case.
   */
  @Override
  public Iterator<VarDef> getVars()
    {
    return bindings_.keySet().iterator();
    }

  /**
   * Returns the number of variable bindings in this test case.
   */
  public int getBindingCount()
    {
    return bindings_.size();
    }

  /**
   * Returns true if the given value is currently bound to the "not applicable" value.
   */
  @Override
  public boolean isNA( VarDef var)
    {
    return getValue( var).isNA();
    }

  /**
   * Returns the variable bound to an invalid value, if any.
   */
  @Override
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
   * Returns the properties of this test case.
   */
  @Override
  public Set<String> getProperties()
    {
    return
      toStream( properties_.getUniqueProperties())
      .sorted( String.CASE_INSENSITIVE_ORDER)
      .collect( toOrderedSet());
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
        VarValueDef value = getValue( var);
        if( !value.isNA())
          {
          ICondition bindingCondition = value.getEffectiveCondition( var.getEffectiveCondition());
          Optional.of( bindingCondition)
            .filter( c -> c instanceof AllOf)
            .map( c -> toStream( ((AllOf) c).getConditions()))
            .orElse( Stream.of( bindingCondition))
            .forEach( c -> conditions.add( c));
          }
        }

      required_ = Cnf.getUnsatisfied( Cnf.convert( conditions), properties_);
      }

    return required_;
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
             && (unsatisfiable = assertions.next()).completable();

           unsatisfiable = null);
      }

    if( unsatisfiable != null)
        {
        logger_.trace( "Infeasible, can no longer satisfy {}, testCase={}", unsatisfiable, this);
        }
    
    return unsatisfiable != null;
    }

  @Override
  public int compareTo( ITestCaseDef other)
    {
    int id = getId()==null? Integer.MAX_VALUE : getId();
    int otherId  = other.getId()==null? Integer.MAX_VALUE : other.getId();
    return id - otherId;
    }

  @Override
  public String toString()
    {
    ArrayList<VarDef> vars = new ArrayList<VarDef>( bindings_.keySet());
    Collections.sort
      ( vars,
        new Comparator<VarDef>()
        {
        @Override
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
      .append( "name", getName())
      .append( "bindings", bindings.toString())
      .append( "properties", properties_)
      .toString();
    }

  private Integer id_;
  private String name_;
  private Map<VarDef,VarValueDef> bindings_ = new LinkedHashMap<VarDef,VarValueDef>();
  private PropertySet properties_ = new PropertySet();
  private IConjunct required_;

  private static final Logger logger_ = LoggerFactory.getLogger( TestCaseDef.class);
  }
