//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 
import java.io.File;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Map;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * Applies an XSLT transform, transforming data written to a {@link #getSource source}
 * stream into results written to a {@link #getTarget target}.
 *
 * @version $Revision$, $Date$
 */
public class TransformFilter implements Runnable, ErrorListener
  {
  /**
   * Receives the input data to be transformed.
   *
   * @version $Revision$, $Date$
   */
  private class TransformSourceStream extends FilterOutputStream
    {
    /**
     * Creates a new TransformSourceStream object.
     */
    public TransformSourceStream( OutputStream source)
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
   * Creates a new TransformFilter object.
   */
  public TransformFilter()
    {
    this( (Source) null);
    }
  
  /**
   * Creates a new TransformFilter object.
   */
  public TransformFilter( Source transform)
    {
    setTransform( transform);
    }
  
  /**
   * Creates a new TransformFilter object.
   */
  public TransformFilter( InputStream transform)
    {
    this( new StreamSource( transform));
    }
  
  /**
   * Creates a new TransformFilter object.
   */
  public TransformFilter( File transform)
    {
    this( new StreamSource( transform));
    }

  /**
   * Changes the source for the XSLT transform document.
   */
  public void setTransform( Source transform)
    {
    close();
    transform_ = transform;
    }

  /**
   * Returns the source for the XSLT transform document.
   */
  protected Source getTransform()
    {
    return transform_;
    }

  /**
   * Changes the set of transform parameter bindings.
   */
  public void setParams( Map<String,Object> params)
    {
    transformParams_ = params;
    }

  /**
   * Returns the set of transform parameter bindings.
   */
  public Map<String,Object> getParams()
    {
    return transformParams_;
    }

  /**
   * Changes the target for transformed output.
   */
  public void setTarget( File target)
    {
    Result result = target==null? null : new StreamResult( target);
    setTarget( result);
    }

  /**
   * Changes the target for transformed output.
   */
  public void setTarget( OutputStream target)
    {
    Result result = target==null? null : new StreamResult( target);
    setTarget( result);
    }

  /**
   * Changes the target for transformed output.
   */
  public void setTarget( Result target)
    {
    close();
    target_ = target;
    }

  /**
   * Returns the target for transformed output.
   */
  public Result getTarget()
    {
    return target_;
    }

  /**
   * Returns the target identifier.
   */
  public String getTargetId()
    {
    String systemId;
    return
      target_ == null?
      null :

      (systemId = target_.getSystemId()) == null?
      "?" :

      systemId;
    }

  /**
   * Returns the source for transform input.
   */
  public OutputStream getSource()
    {
    if( source_ == null)
      {
      start();
      }
    
    return source_;
    }

  /**
   * Starts transform output.
   */
  private void start()
    {
    try
      {
      // Create Transformer object.
      Source transform = getTransform();
      if( transform == null)
        {
        throw new IllegalStateException( "No XSLT transform specified");
        }
      TransformerFactory factory = TransformerFactory.newInstance();
      factory.setErrorListener( this);
      
      transformer_ = factory.newTransformer( transform);
      transformer_.setErrorListener( this);
      if( transformParams_ != null)
        {
        for( String paramName : transformParams_.keySet())
          {
          transformer_.setParameter( paramName, transformParams_.get( paramName));
          }
        }

      // Create filter streams.
      PipedOutputStream pipeOut = new PipedOutputStream();
      PipedInputStream pipeIn = new PipedInputStream( pipeOut);
      input_ = new StreamSource( pipeIn);
      source_ = new TransformSourceStream( pipeOut);

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
   * Terminates transform output.
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
      logger_.error( "Can't complete transform", e);
      }
    }

  /**
   * Completes transform output.
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
        : new IOException( "Can't write output to target=" + getTargetId(), failure_);
      
      failure_ = null;
      input_ = null;
      source_ = null;
      target_ = null;
      thread_ = null;
      transform_ = null;
      transformer_ = null;

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
      // If no target defined, write to standard output.
      Result target =
        target_ == null
        ? new StreamResult( System.out)
        : target_;
      
      transformer_.transform( input_, target);
      }
    catch( Exception e)
      {
      failure_ = e;
      logger_.error( "Can't complete transform", e);
      }
    finally
      {
      OutputStream targetStream =
        target_ instanceof StreamResult
        ? ((StreamResult) target_).getOutputStream()
        : null;

      IOUtils.closeQuietly( targetStream);
      }
    
    logger_.debug( "Completed, thread={}", thread_);
    }

  public void error( TransformerException exception) throws TransformerException
    {
    logger_.error( "target={}, {}", getTargetId(), exception);
    throw exception;
    }
  
  public void fatalError( TransformerException exception) throws TransformerException
    {
    logger_.error( "target={}, {}", getTargetId(), exception);
    throw exception;
    }
  
 public void warning( TransformerException exception)
    {
    logger_.warn( "target={}, {}", getTargetId(), exception);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "target", getTargetId())
      .toString();
    }
  
  private Exception failure_;
  private StreamSource input_;
  private OutputStream source_;
  private Result target_;
  private Thread thread_;
  private Source transform_;
  private Transformer transformer_;
  private Map<String,Object> transformParams_;

  private static final Logger logger_ = LoggerFactory.getLogger( TransformFilter.class);
  }
