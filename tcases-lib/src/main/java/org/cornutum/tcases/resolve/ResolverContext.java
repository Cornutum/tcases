//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.ExecutionNotifier;
import org.cornutum.tcases.util.Notifier;

import java.util.Iterator;
import java.util.Optional;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * Defines options used to resolve an executable API test case.
 */
public class ResolverContext extends ExecutionNotifier<ResolverException>
  {
  /**
   * Creates a new ResolverContext instance.
   */
  public ResolverContext( String... startLocation)
    {
    super( startLocation);
    setRandom( new Random());
    setNotifier( Notifier.ignore());
    setMaxTries( 10000);
    }
  
  /**
   * Returns an exception to throw for the given failure.
   */
  @Override
  protected ResolverException whenFailure( Throwable e)
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
  public <T> T tryUntil( Supplier<Optional<T>> valueSupplier) throws ResolverSkipException
    {
    return tryUntil( Stream.generate( () -> valueSupplier.get()));
    }

  /**
   * Returns the first result that is present. If the {@link #getMaxTries maximum tries} have been
   * attempted unsuccessfully reports a failure. Otherwise, returns the result.
   */
  public <T> T tryUntil( Stream<Optional<T>> results) throws ResolverSkipException
    {
    Iterator<Optional<T>> supplier = results.iterator();
    T result = null;
    int tries;

    for( tries = 0;

         tries < getMaxTries()
           && supplier.hasNext()
           && (result = supplier.next().orElse( null)) == null;

         tries++);

    if( result == null)
      {
      throw new ResolverSkipException( getLocation(), String.format( "Unable to resolve a value after %s tries", tries));
      }

    return result;
    }

  /**
   * Returns a new ResolverContext builder.
   */
  public static Builder builder( String... startLocation)
    {
    return new Builder( startLocation);
    }
  
  /**
   * Builds a new {@link ResolverContext} instance.
   */
  public static class Builder
    {
    public Builder( String... startLocation)
      {
      resolverContext_ = new ResolverContext( startLocation);
      }

    public Builder random( Random random)
      {
      resolverContext_.setRandom( random);
      return this;
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
  private int maxTries_;
  }
