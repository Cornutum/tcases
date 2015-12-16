//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.util.ToString;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
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
 */
public class TransformFilter extends AbstractFilter implements ErrorListener
  {
  /**
   * Creates a new TransformFilter object.
   */
  public TransformFilter()
    {
    this( (Source) null, null);
    }
  
  /**
   * Creates a new TransformFilter to apply the given XSLT transform.
   */
  public TransformFilter( Source transform)
    {
    this( transform, null);
    }
  
  /**
   * Creates a new TransformFilter to apply the given XSLT transform.
   */
  public TransformFilter( InputStream transform)
    {
    this( new StreamSource( transform), null);
    }
  
  /**
   * Creates a new TransformFilter to apply the given XSLT transform.
   */
  public TransformFilter( File transform)
    {
    this( new StreamSource( transform), null);
    }
  
  /**
   * Creates a new TransformFilter to apply the given XSLT transform.
   */
  public TransformFilter( Source transform, Map<String,Object> params)
    {
    setTransform( transform);
    setParams( params);
    }
  
  /**
   * Creates a new TransformFilter to apply the given XSLT transform.
   */
  public TransformFilter( InputStream transform, Map<String,Object> params)
    {
    this( new StreamSource( transform), params);
    }
  
  /**
   * Creates a new TransformFilter to apply the given XSLT transform.
   */
  public TransformFilter( File transform, Map<String,Object> params)
    {
    this( new StreamSource( transform), params);
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
   * Initializes the filter.
   */
  protected void initializeFilter( InputStream filterInput, OutputStream filterOutput)
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
      
      transformSource_ = new StreamSource( filterInput);
      transformResult_ = new StreamResult( filterOutput);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't initialize filter", e);
      } 
    }

  /**
   * Reads data to be transformed from the {@link #getFilterInput filter input stream} and
   * write transformed data to the {@link #getFilterOutput filter output stream}.
   */
  protected void applyFilter() throws Exception
    {
    transformer_.transform( transformSource_, transformResult_);
    }

  public void error( TransformerException exception) throws TransformerException
    {
    logger_.error( "Exception during transform, {}", exception);
    throw exception;
    }
  
  public void fatalError( TransformerException exception) throws TransformerException
    {
    logger_.error( "Exception during transform, {}", exception);
    throw exception;
    }
  
 public void warning( TransformerException exception)
    {
    logger_.warn( "Exception during transform, {}", exception);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "transform", transform_==null? null : transform_.getSystemId())
      .toString();
    }
  
  private StreamSource transformSource_;
  private Result transformResult_;
  private Source transform_;
  private Transformer transformer_;
  private Map<String,Object> transformParams_;

  private static final Logger logger_ = LoggerFactory.getLogger( TransformFilter.class);
  }
