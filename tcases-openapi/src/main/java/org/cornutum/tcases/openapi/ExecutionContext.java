//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.util.CollectionUtils.*;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Supplier;

/**
 * Provides a context for exceptions that occur during an execution.
 */
public abstract class ExecutionContext<E extends RuntimeException>
  {
  /**
   * Returns the result of the given supplier within the specified context.
   */
  public <T> T resultFor( String context, Supplier<T> supplier)
    {
    context_.addLast( context);
    try
      {
      return supplier.get();
      }
    catch( Exception e)
      {
      throw whenFailure( e);
      }
    finally
      {
      context_.removeLast();
      }
    }

  /**
   * Performs the given action within the specified context.
   */
  public void doFor( String context, Runnable action)
    {
    context_.addLast( context);
    try
      {
      action.run();
      }
    catch( Exception e)
      {
      throw whenFailure( e);
      }
    finally
      {
      context_.removeLast();
      }
    }

  /**
   * Returns an exception to throw for the given failure.
   */
  protected abstract E whenFailure( Exception e);

  /**
   * Returns the path to the current context.
   */
  public String[] getLocation()
    {
    return toStream( context_.iterator()).toArray( String[]::new);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getLocation())
      .toString();
    }

  private Deque<String> context_ = new ArrayDeque<String>();
  }
