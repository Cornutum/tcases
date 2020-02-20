//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
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
 * Provides a context for exceptions that occur when processing an OpenAPI specification
 */
public class OpenApiContext
  {
  /**
   * Creates a new OpenApiContext instance.
   */
  public OpenApiContext()
    {
    }

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
    catch( OpenApiException oae)
      {
      throw oae;
      }
    catch( Exception e)
      {
      throw new OpenApiException( getLocation(), e);
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
    catch( OpenApiException oae)
      {
      throw oae;
      }
    catch( Exception e)
      {
      throw new OpenApiException( getLocation(), e);
      }
    finally
      {
      context_.removeLast();
      }
    }

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
