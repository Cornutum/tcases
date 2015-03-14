//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.iterators.SingletonIterator;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A {@link org.cornutum.tcases.conditions.ICondition condition} that defined by the disjunction (logical OR) of one or
 * more {@link IAssertion assertions}.
 *
 */
public class Disjunction implements IDisjunct
  {
  /**
   * Creates a new Disjunction instance.
   */
  public Disjunction( IAssertion ... assertions)
    {
    for( int i = 0; i < assertions.length; i++)
      {
      add( assertions[i]);
      }
    }
  
  /**
   * Creates a new Disjunction instance.
   */
  public Disjunction( IDisjunct ... disjuncts)
    {
    for( int i = 0; i < disjuncts.length; i++)
      {
      add( disjuncts[i]);
      }
    }
  
  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  public boolean satisfied( PropertySet properties)
    {
    boolean isSatisfied;
    Iterator<IAssertion> assertions;
    
    for( assertions = getAssertions(),
           isSatisfied = !assertions.hasNext();

         !isSatisfied
           && assertions.hasNext();

         isSatisfied = assertions.next().satisfied( properties));
    
    return isSatisfied;
    }

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  public boolean compatible( PropertySet properties)
    {
    boolean isCompatible;
    Iterator<IAssertion> assertions;
    
    for( assertions = getAssertions(),
           isCompatible = !assertions.hasNext();

         !isCompatible
           && assertions.hasNext();

         isCompatible = assertions.next().compatible( properties));
    
    return isCompatible;
    }
  
  /**
   * Implements the Visitor pattern for this condition.
   */
  public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }
  
  /**
   * Adds an assertion to this disjunction.
   */
  public Disjunction add( IAssertion assertion)
    {
    assertions_.add( assertion);
    return this;
    }
  
  /**
   * Adds all assertions for the given IDisjunct to this disjunction.
   */
  public Disjunction add( IDisjunct disjunct)
    {
    for( Iterator<IAssertion> assertions = disjunct.getAssertions();
         assertions.hasNext();)
      {
      add( assertions.next());
      }
    return this;
    }

  /**
   * Removes an assertion from this disjunction.
   */
  public Disjunction remove( IAssertion assertion)
    {
    assertions_.remove( assertion);
    return this;
    }

  /**
   * Returns the assertions in this disjunction.
   */
  public Iterator<IAssertion> getAssertions()
    {
    return assertions_.iterator();
    }

  /**
   * Returns true if the given assertion is a member of this disjunction.
   */
  public boolean contains( IAssertion assertion)
    {
    return assertions_.contains( assertion);
    }

  /**
   * Returns the number of assertions for this disjunction.
   */
  public int getAssertionCount()
    {
    return assertions_.size();
    }

  /**
   * Returns the disjuncts in this conjunction.
   */
  public Iterator<IDisjunct> getDisjuncts()
    {
    return new SingletonIterator<IDisjunct>( this);
    }

  /**
   * Returns the number of disjunctions for this conjunction.
   */
  public int getDisjunctCount()
    {
    return 1;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( assertions_)
      .toString();
    }

  public int hashCode()
    {
    return assertions_.hashCode();
    }

  public boolean equals( Object object)
    {
    IDisjunct other =
      object != null && object instanceof IDisjunct
      ? (IDisjunct) object
      : null;
    
    return
      other != null
      && assertions_.equals( new HashSet<IDisjunct>( IteratorUtils.toList( other.getAssertions())));
    }

  private Set<IAssertion> assertions_ = new HashSet<IAssertion>();
  }

