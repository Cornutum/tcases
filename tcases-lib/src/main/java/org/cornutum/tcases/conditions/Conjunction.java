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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * A {@link ICondition condition} in conjunctive normal form.
 *
 */
public class Conjunction implements IConjunct
  {
  /**
   * Creates a new Conjunction instance.
   */
  public Conjunction( IDisjunct ... disjuncts)
    {
    for( int i = 0; i < disjuncts.length; i++)
      {
      add( disjuncts[i]);
      }
    }
  
  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  @Override
public boolean satisfied( PropertySet properties)
    {
    boolean isSatisfied;
    Iterator<IDisjunct> disjuncts;
    
    for( disjuncts = getDisjuncts(),
           isSatisfied = true;
           

         isSatisfied
           && disjuncts.hasNext();

         isSatisfied = disjuncts.next().satisfied( properties));
    
    return isSatisfied;
    }

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  @Override
public boolean compatible( PropertySet properties)
    {
    boolean isCompatible;
    Iterator<IDisjunct> disjuncts;
    
    for( disjuncts = getDisjuncts(),
           isCompatible = true;
           

         isCompatible
           && disjuncts.hasNext();

         isCompatible = disjuncts.next().compatible( properties));
    
    return isCompatible;
    }
  
  /**
   * Implements the Visitor pattern for this condition.
   */
  @Override
public void accept( IConditionVisitor visitor)
    {
    visitor.visit( this);
    }
  
  /**
   * Adds a disjunct to this conjunction.
   */
  public Conjunction add( IDisjunct disjunct)
    {
    if( disjunct.getAssertionCount() > 0)
      {
      disjuncts_.add( disjunct);
      }
    return this;
    }
  
  /**
   * Appends another conjunction to this conjunction.
   */
  public Conjunction append( IConjunct conjunct)
    {
    for( Iterator<IDisjunct> disjuncts = conjunct.getDisjuncts();
         disjuncts.hasNext();
         add( disjuncts.next()));
    return this;
    }

  /**
   * Removes a disjunct from this conjunction.
   */
  public Conjunction remove( IDisjunct disjunct)
    {
    disjuncts_.remove( disjunct);
    return this;
    }

  /**
   * Returns the disjuncts in this conjunction.
   */
  @Override
public Iterator<IDisjunct> getDisjuncts()
    {
    return disjuncts_.iterator();
    }

  /**
   * Returns the number of disjunctions for this conjunction.
   */
  @Override
public int getDisjunctCount()
    {
    return disjuncts_.size();
    }

  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( disjuncts_)
      .toString();
    }

  @Override
public int hashCode()
    {
    return disjuncts_.hashCode();
    }

  @Override
public boolean equals( Object object)
    {
    IConjunct other =
      object != null && object instanceof IConjunct
      ? (IConjunct) object
      : null;
    
    return
      other != null
      && disjuncts_.equals( new HashSet<IDisjunct>( IteratorUtils.toList( other.getDisjuncts())));
    }

  private Set<IDisjunct> disjuncts_ = new HashSet<IDisjunct>();
  }

