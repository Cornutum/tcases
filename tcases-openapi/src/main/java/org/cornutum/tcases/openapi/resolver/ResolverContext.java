//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.ExecutionContext;
import org.cornutum.tcases.openapi.Notifier;

import java.util.Optional;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * Defines options used to resolve an executable API test case.
 */
public class ResolverContext extends ExecutionContext<ResolverException>
  {
  /**
   * Creates a new ResolverContext instance.
   */
  public ResolverContext( Random random)
    {
    setRandom( random);
    setNotifier( null);
    setMaxTries( 10000);
    }
  
  /**
   * Returns an exception to throw for the given failure.
   */
  protected ResolverException whenFailure( Exception e)
    {
    return
      ResolverException.class.isAssignableFrom( e.getClass())
      ? (ResolverException) e
      : new ResolverException( getLocation(), e);
    }

  /**
   * Changes the random number generator used to resolve test cases.
   */
  public void setRandom( Random random)
    {
    random_ =
      Optional.ofNullable( random)
      .orElseThrow( () -> new IllegalStateException( "Random number generator must be defined"));
    }

  /**
   * Returns the random number generator used to resolve test cases.
   */
  public Random getRandom()
    {
    return random_;
    }
  
  /**
   * Changes the notifier that reports conditions that occur when resolving a test case.
   */
  public void setNotifier( Notifier notifier)
    {
    notifier_ =
      notifier == null
      ? ResolverConditionNotifier.fail()
      : notifier;
    }

  /**
   * Returns the notifier that reports conditions that occur when resolving a test case.
   */
  public Notifier getNotifier()
    {
    return notifier_;
    }

  /**
   * Changes the maximum attempts to resolve an input value before reporting failure.
   */
  public void setMaxTries( int maxTries)
    {
    maxTries_ = maxTries;
    }

  /**
   * Returns the maximum attempts to resolve an input value before reporting failure.
   */
  public int getMaxTries()
    {
    return maxTries_;
    }

  /**
   * Repeats evaluation of the given <CODE>valueSupplier</CODE> until a result is present or until
   * the {@link #getMaxTries maximum tries} have been attempted. If the no more attempts are possible,
   * reports a failure. Otherwise, returns the supplied result.
   */
  public <T> T tryUntil( Supplier<Optional<T>> valueSupplier)
    {
    return
      Stream.generate( () -> valueSupplier.get())
      .limit( getMaxTries())
      .filter( Optional::isPresent)
      .findFirst()
      .map( Optional::get)
      .orElseThrow( () -> new ResolverSkipException( getLocation(), String.format( "Unable to resolve a value after %s tries", getMaxTries())));
    }

  /**
   * Reports a condition that will affect the expected test cat.
   *
   * @param reason  A description of the condition
   */
  public void warn( String reason)
    {
    notifier_.warn( getLocation(), reason);
    }

  /**
   * Reports an error that would have resulted in an inconsistent or infeasible test case.
   *
   * @param reason  A description of the problem
   * @param resolution  A description of how the problem was resolved
   */
  public void error( String reason, String resolution)
    {
    notifier_.error( getLocation(), reason, resolution);
    }

  /**
   * Returns a new ResolverContext builder.
   */
  public static Builder builder( Random random)
    {
    return new Builder( random);
    }
  
  /**
   * Builds a new {@link ResolverContext} instance.
   */
  public static class Builder
    {
    public Builder( Random random)
      {
      resolverContext_ = new ResolverContext( random);
      }

    public Builder notifier( Notifier notifier)
      {
      resolverContext_.setNotifier( notifier);
      return this;
      }

    public Builder maxTries( int maxTries)
      {
      resolverContext_.setMaxTries( maxTries);
      return this;
      }

    public ResolverContext build()
      {
      return resolverContext_;
      }
      
    private ResolverContext resolverContext_;
    }

  private Random random_;
  private Notifier notifier_;
  private int maxTries_;
  }
