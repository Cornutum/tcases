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
   * A {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * all of the given properties.
   */
  public static ContainsAll has( String... properties)
    {
    return new ContainsAll( properties);
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * any of the given properties.
   */
  public static ContainsAny hasAny( String... properties)
    {
    return new ContainsAny( properties);
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * all of the given properties.
   */
  public static AllOf allOf( String... properties)
    {
    return allOf( has( properties));
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * any of the given properties.
   */
  public static AnyOf anyOf( String... properties)
    {
    return anyOf( hasAny( properties));
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link org.cornutum.tcases.PropertySet} that contains
   * none of the given properties.
   */
  public static Not not( String... properties)
    {
    return not( hasAny( properties));
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

    public Stream<String> propertiesVisited()
      {
      return refBuilder_.build().distinct();
      }
    }
  }

