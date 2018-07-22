//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

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
   * A {@link ICondition condition} that is satisfied by a {@link PropertySet} that contains
   * all of the given properties.
   */
  public static ContainsAll has( String... properties)
    {
    return new ContainsAll( properties);
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link PropertySet} that contains
   * any of the given properties.
   */
  public static ContainsAny hasAny( String... properties)
    {
    return new ContainsAny( properties);
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link PropertySet} that contains
   * all of the given properties.
   */
  public static AllOf allOf( String... properties)
    {
    return allOf( has( properties));
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link PropertySet} that contains
   * any of the given properties.
   */
  public static AnyOf anyOf( String... properties)
    {
    return anyOf( hasAny( properties));
    }

  /**
   * A {@link ICondition condition} that is satisfied by a {@link PropertySet} that contains
   * none of the given properties.
   */
  public static Not not( String... properties)
    {
    return not( hasAny( properties));
    }
  }

