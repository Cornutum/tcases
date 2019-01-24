//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

/**
 * Transforms data written to a {@link #getSource source}
 * stream into results written to a {@link #getTarget target}.
 *
 */
public abstract class AbstractFilter implements Runnable
  {
  /**
   * Receives the input data to be transformed.
   *
   */
  private class FilterSourceStream extends FilterOutputStream
    {
    /**
     * Creates a new FilterSourceStream object.
     */
    public FilterSourceStream( OutputStream source)
      {
      super( source);
      }

    public void close() throws IOException
      {
      super.close();
      complete();
      }
    }

  /**
   * Changes the target for filter output.
   */
  public void setTarget( File target)
    {
    try
      {
      OutputStream targetStream = target==null? null : new FileOutputStream( target);
      setTarget( targetStream);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't create target stream", e);
      }
    }

  /**
   * Changes the target for filter output.
   */
  public void setTarget( OutputStream target)
    {
    close();
    target_ = target;
    }

  /**
   * Returns the target for filter output.
   */
  public OutputStream getTarget()
    {
    return target_;
    }

  /**
   * Returns the source for filter input.
   */
  public OutputStream getSource()
    {
    if( source_ == null)
      {
      source_ = initializeSource();
      start();
      }
    
    return source_;
    }

  /**
   * Initializes the source for filter input.
   */
  private OutputStream initializeSource()
    {
    try
      {
      PipedOutputStream pipeOut = new PipedOutputStream();
      PipedInputStream pipeIn = new PipedInputStream( pipeOut);

      filterInput_ = pipeIn;
      return new FilterSourceStream( pipeOut);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't initialize source stream", e);
      }
    }

  /**
   * Initializes the filter.
   */
  protected void initializeFilter( InputStream filterInput, OutputStream filterOutput)
    {
    // By default, nothing to do.
    }

  /**
   * Returns the stream that provides input to the filter.
   */
  protected InputStream getFilterInput()
    {
    return filterInput_;
    }

  /**
   * Returns the stream that provides output from the filter.
   */
  protected OutputStream getFilterOutput()
    {
    OutputStream target = getTarget();
    return target == null? System.out : target;
    }

  /**
   * Reads data to be transformed from the {@link #getFilterInput filter input stream} and
   * write transformed data to the {@link #getFilterOutput filter output stream}.
   */
  protected abstract void applyFilter() throws Exception;

  /**
   * Starts filter output.
   */
  private void start()
    {
    try
      {
      // Initialize filter.
      initializeFilter( getFilterInput(), getFilterOutput());

      // Start filter thread.
      failure_ = null;
      thread_ = new Thread( this, String.valueOf( this));
      thread_.start();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't start filtering", e);
      } 
    }

  /**
   * Terminates filter output.
   */
  public void close()
    {
    IOUtils.closeQuietly( source_);
    try
      {
      complete();
      }
    catch( Exception e)
      {
      logger_.error( "Can't complete filtering", e);
      }
    }

  /**
   * Completes filter output.
   */
  private void complete() throws IOException
    {
    if( source_ != null)
      {
      try
        {
        thread_.join();
        }
      catch( Exception e)
        {
        logger_.error( "Thread=" + thread_ + " not completed", e);
        }

      IOException failure =
        failure_ == null
        ? null
        : new IOException( "Can't write filter output", failure_);
      
      failure_      = null;
      filterInput_  = null;
      source_       = null;
      target_       = null;
      thread_       = null;

      if( failure != null)
        {
        throw failure;
        }
      }
    }  

  public void run()
    {
    logger_.debug( "Starting, thread={}", thread_);
    
    try
      {
      applyFilter();
      }
    catch( Exception e)
      {
      failure_ = e;
      logger_.error( "Can't complete filter", e);
      }
    finally
      {
      IOUtils.closeQuietly( target_);
      }
    
    logger_.debug( "Completed, thread={}", thread_);
    }
  
  private Exception failure_;
  private OutputStream source_;
  private OutputStream target_;
  private InputStream filterInput_;
  private Thread thread_;

  private static final Logger logger_ = LoggerFactory.getLogger( AbstractFilter.class);
  }
