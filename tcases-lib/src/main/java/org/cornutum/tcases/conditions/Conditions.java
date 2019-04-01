//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.stream.Stream;

/**
 * Defines methods for handling conditions.
 */
public final class Conditions
  {
  private Conditions()
    {
    // Static methods only
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied if and only if all of
   * the given conditions are satisfied. 
   */
  public static AllOf allOf( ICondition... conditions)
    {
    return new AllOf( conditions);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied if and only if any of
   * the given conditions are satisfied. 
   */
  public static AnyOf anyOf( ICondition... conditions)
    {
    return new AnyOf( conditions);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied if and only if none of
   * the given conditions are satisfied. 
   */
  public static Not not( ICondition... conditions)
    {
    return new Not( conditions);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * all of the given properties.
   */
  public static ContainsAll has( String... properties)
    {
    return new ContainsAll( properties);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * any of the given properties.
   */
  public static ContainsAny hasAny( String... properties)
    {
    return new ContainsAny( properties);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * all of the given properties.
   */
  public static AllOf allOf( String... properties)
    {
    return allOf( has( properties));
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * any of the given properties.
   */
  public static AnyOf anyOf( String... properties)
    {
    return anyOf( hasAny( properties));
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * none of the given properties.
   */
  public static Not not( String... properties)
    {
    return not( hasAny( properties));
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * more than a specified minimum that the number of instances of a property.
   */
  public static AssertMore moreThan( String property, int minimum)
    {
    return new AssertMore( property, minimum);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * less than a specified maximum that the number of instances of a property.
   */
  public static AssertLess lessThan( String property, int maximum)
    {
    return new AssertLess( property, maximum);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * less than or equal to a specified maximum that the number of instances of a property.
   */
  public static AssertNotMore notMoreThan( String property, int maximum)
    {
    return new AssertNotMore( property, maximum);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * greater than or equal to a specified minimum that the number of instances of a property.
   */
  public static AssertNotLess notLessThan( String property, int minimum)
    {
    return new AssertNotLess( property, minimum);
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * between a specified minimum (inclusive) and maximum (inclusive) number of instances of a property.
   */
  public static Between between( String property, int minimum, int maximum)
    {
    return new Between( notLessThan( property, minimum), notMoreThan( property, maximum));
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * between a specified minimum (exclusive) and maximum (exclusive) number of instances of a property.
   */
  public static Between betweenExclusive( String property, int minimum, int maximum)
    {
    return new Between( moreThan( property, minimum), lessThan( property, maximum));
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * between a specified minimum (exclusive) and maximum (inclusive) number of instances of a property.
   */
  public static Between betweenExclusiveMin( String property, int minimum, int maximum)
    {
    return new Between( moreThan( property, minimum), notMoreThan( property, maximum));
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * between a specified minimum (inclusive) and maximum (exclusive) number of instances of a property.
   */
  public static Between betweenExclusiveMax( String property, int minimum, int maximum)
    {
    return new Between( notLessThan( property, minimum), lessThan( property, maximum));
    }

  /**
   * Returns a {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * a specified number of instances of a property.
   */
  public static Equals equalTo( String property, int count)
    {
    return new Equals( property, count);
    }

  /**
   * Returns the properties referenced by the given condition.
   */
  public static Stream<String> propertiesReferenced( ICondition condition)
    {
    if( condition == null)
      {
      return Stream.empty();
      }
    
    PropertyVisitor propertyVisitor = new PropertyVisitor();
    condition.accept( propertyVisitor);
    return propertyVisitor.propertiesVisited();
    }

  /**
   * An {@link IConditionVisitor} that creates a stream of properties referenced.
   */
  private static class PropertyVisitor implements IConditionVisitor
    {
    private Stream.Builder<String> refBuilder_ = Stream.builder();

    public void visit( AllOf condition)
      {
      toStream( condition.getConditions()).flatMap( c -> propertiesReferenced( c)).forEach( p -> refBuilder_.add( p));
      }
  
    public void visit( AnyOf condition)
      {
      toStream( condition.getConditions()).flatMap( c -> propertiesReferenced( c)).forEach( p -> refBuilder_.add( p));
      }
  
    public void visit( ContainsAll condition)
      {
      toStream( condition.getProperties()).forEach( p -> refBuilder_.add( p));
      }
  
    public void visit( ContainsAny condition)
      {
      toStream( condition.getProperties()).forEach( p -> refBuilder_.add( p));
      }
  
    public void visit( IConjunct condition)
      {
      // NA
      }
  
    public void visit( Not condition)
      {
      toStream( condition.getConditions()).flatMap( c -> propertiesReferenced( c)).forEach( p -> refBuilder_.add( p));
      }

    public void visit( AssertLess condition)
      {
      visitBoundedAssertion( condition);
      }

    public void visit( AssertMore condition)
      {
      visitBoundedAssertion( condition);
      }

    public void visit( AssertNotLess condition)
      {
      visitBoundedAssertion( condition);
      }

    public void visit( AssertNotMore condition)
      {
      visitBoundedAssertion( condition);
      }

    public void visit( Between condition)
      {
      visit( (AllOf) condition);
      }

    public void visit( Equals condition)
      {
      visit( (AllOf) condition);
      }

    private void visitBoundedAssertion( BoundedAssertion condition)
      {
      refBuilder_.add( condition.getProperty());
      }

    public Stream<String> propertiesVisited()
      {
      return refBuilder_.build().distinct();
      }
    }
  }

