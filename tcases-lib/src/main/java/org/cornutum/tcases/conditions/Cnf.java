//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.PropertySet;

import org.apache.commons.collections4.IteratorUtils;

import java.util.Iterator;

/**
 * Defines methods for handling conditions in conjunctive normal form.
 *
 */
public abstract class Cnf
  {
  /**
   * Converts a {@link ICondition condition} into conjunctive normal form.
   *
   */
  public static class Converter implements IConditionVisitor
    {
    /**
     * Converts the given condition into conjunctive normal form.
     */
    public IConjunct convert( ICondition condition)
      {
      conjunct_ = null;
      condition.accept( this);
      return refactor( conjunct_);
      }
    
    public void visit( AllOf condition)
      {
      Conjunction conjunction = new Conjunction();
      for( Iterator<ICondition> conditions = condition.getConditions();
           conditions.hasNext();)
        {
        IConjunct conjunct = Cnf.convert( conditions.next());
        for( Iterator<IDisjunct> disjuncts = conjunct.getDisjuncts();
             disjuncts.hasNext();)
          {
          IDisjunct disjunct = disjuncts.next();
          if( !isTautology( disjunct))
            {
            conjunction.add( disjunct);
            }
          }
        }
      conjunct_ = simplify( conjunction);
      }
  
    public void visit( AnyOf condition)
      {
      IConjunct conjunct = new Conjunction();
      for( Iterator<ICondition> conditions = condition.getConditions();
           conditions.hasNext();
           conjunct = either( conjunct, Cnf.convert( conditions.next())));

      conjunct_ = simplify( conjunct);
      }
  
    public void visit( ContainsAll condition)
      {
      Conjunction conjunction = new Conjunction();
      for( Iterator<String> properties = condition.getProperties();
           properties.hasNext();)
        {
        conjunction.add( new Assert( properties.next()));
        }

      conjunct_ = simplify( conjunction);
      }
  
    public void visit( ContainsAny condition)
      {
      Disjunction disjunction = new Disjunction();
      for( Iterator<String> properties = condition.getProperties();
           properties.hasNext();)
        {
        disjunction.add( new Assert( properties.next()));
        }
    
      conjunct_ = simplify( disjunction);
      }
  
    public void visit( IConjunct condition)
      {
      conjunct_ = condition;
      }
  
    public void visit( Not condition)
      {
      Conjunction conjunction = new Conjunction();
      for( Iterator<ICondition> conditions = condition.getConditions();
           conditions.hasNext();)
        {
        IConjunct conjunct = negate( Cnf.convert( conditions.next()));
        for( Iterator<IDisjunct> disjuncts = conjunct.getDisjuncts();
             disjuncts.hasNext();)
          {
          IDisjunct disjunct = disjuncts.next();
          if( !isTautology( disjunct))
            {
            conjunction.add( disjunct);
            }
          }
        }
    
      conjunct_ = simplify( conjunction);
      }

      private IConjunct conjunct_;
    }

  /**
   * Returns the negation of the given CNF condition.
   */
  public static IConjunct negate( IConjunct conjunct)
    {
    AnyOf anyOf = new AnyOf();
    for( Iterator<IDisjunct> disjuncts = conjunct.getDisjuncts();
           disjuncts.hasNext();)
        {
        Conjunction conjunction = new Conjunction();
        for( Iterator<IAssertion> assertions = disjuncts.next().getAssertions();
             assertions.hasNext();)
          {
          conjunction.add( assertions.next().negate());
          }

        anyOf.add( simplify( conjunction));
        }
    
    return Cnf.convert( anyOf);
    }

  /**
   * Returns the logical OR of the given CNF conditions.
   */
  public static IConjunct either( IConjunct conjunct1, IConjunct conjunct2)
    {
    IConjunct conjunct;

    if( conjunct1.getDisjunctCount() == 0)
      {
      conjunct = conjunct2;
      }

    else if( conjunct2.getDisjunctCount() == 0)
      {
      conjunct = conjunct1;
      }

    else
      {
      Conjunction conjunction = new Conjunction();
      for( Iterator<IDisjunct> disjuncts1 = conjunct1.getDisjuncts();
           disjuncts1.hasNext();)
        {
        IDisjunct disjunct1 = disjuncts1.next();
        
        for( Iterator<IDisjunct> disjuncts2 = conjunct2.getDisjuncts();
           disjuncts2.hasNext();)
          {
          IDisjunct disjunct = new Disjunction( disjunct1, disjuncts2.next());
          if( !isTautology( disjunct))
            {
            conjunction.add( simplify( disjunct));
            }
          }
        }
      
      conjunct = simplify( conjunction);
      }

    return conjunct;
    }

  /**
   * Return refactored conjunction formed by removing superfluous terms.
   */
  public static IConjunct refactor( IConjunct conjunct)
    {
    // Is there a single assertion term?
    Iterator<IDisjunct> disjuncts = conjunct.getDisjuncts();
    IAssertion assertion = null;
    while( disjuncts.hasNext() && (assertion = toAssertion( disjuncts.next())) == null);

    return
      assertion == null
      // No, return original conjunction
      ? conjunct

      // Yes, return refactored conjunction
      : simplify
        ( new Conjunction( assertion)
          .append( refactor( remainder( conjunct, assertion))));
    }

  /**
   * Returns the remainder after removing all terms that contain the given assertion.
   */
  public static IConjunct remainder( IConjunct conjunct, IAssertion assertion)
    {
    Conjunction rest = new Conjunction();
    for( Iterator<IDisjunct> disjuncts = conjunct.getDisjuncts();
         disjuncts.hasNext();)
      {
      IDisjunct disjunct = disjuncts.next();
      if( !disjunct.contains( assertion))
        {
        rest.add( disjunct);
        }
      }
    
    return rest;
    }

  /**
   * If the given disjunction consists of a single assertion, returns the
   * equivalent assertion. Otherwise, return null.
   */
  public static IAssertion toAssertion( IDisjunct disjunct)
    {
    return
      disjunct.getAssertionCount() == 1
      ? disjunct.getAssertions().next()
      : null;
    }

  /**
   * Returns the simple form of the given conjunction.
   */
  public static IConjunct simplify( IConjunct conjunct)
    {
    return
      conjunct.getDisjunctCount() == 1
      ? conjunct.getDisjuncts().next()
      : conjunct;
    }

  /**
   * Returns the simple form of the given disjunction.
   */
  public static IDisjunct simplify( IDisjunct disjunct)
    {
    IAssertion assertion = toAssertion( disjunct);
    return
      assertion == null
      ? disjunct
      : assertion;
    }

  /**
   * Returns true if the given disjunction is universally true.
   */
  public static boolean isTautology( IDisjunct disjunct)
    {
    boolean tautology = false;
    if( disjunct != null)
      {
      IAssertion[] assertions = IteratorUtils.toArray( disjunct.getAssertions(), IAssertion.class);
      int max = assertions.length;
      int maxCompared = max - 1;
      
      for( int compared = 0;
           !tautology && compared < maxCompared;
           compared++)
        {
        IAssertion assertCompared = assertions[ compared];
        
        for( int other = compared + 1;
             !tautology && other < max;
             tautology = assertCompared.negates( assertions[ other++]));
        }
      }
    
    return tautology;
    }

  /**
   * Converts the given condition into conjunctive normal form.
   */
  public static IConjunct convert( ICondition condition)
    {
    return new Converter().convert( condition);
    }

  /**
   * Returns true if the given properties partially satisfy the given condition.
   */
  public static boolean satisfiesSome( IConjunct condition, PropertySet properties)
    {
    boolean satisfies;
    Iterator<IDisjunct> disjuncts;
    for( disjuncts = condition.getDisjuncts(),
           satisfies = !disjuncts.hasNext();
         
         !satisfies
           && disjuncts.hasNext();

         satisfies = disjuncts.next().satisfied( properties));

    return satisfies;
    }

  /**
   * Returns the part of the given condition unsatisfied by the given properties.
   */
  public static IConjunct getUnsatisfied( IConjunct condition, PropertySet properties)
    {
    Conjunction unsatisfied = new Conjunction();
    for( Iterator<IDisjunct> disjuncts = condition.getDisjuncts();
         disjuncts.hasNext();)
      {
      IDisjunct disjunct = disjuncts.next();
      if( !disjunct.satisfied( properties))
        {
        unsatisfied.add( disjunct);
        }
      }

    return unsatisfied;
    }
  }

