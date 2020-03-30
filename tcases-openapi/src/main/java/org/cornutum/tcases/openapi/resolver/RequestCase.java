//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.net.URI;
import java.util.List;
import java.util.Optional;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

/**
 * Describes an executable test case for an API request.
 */
public class RequestCase
  {
  /**
   * Creates a new RequestCase instance.
   */
  public RequestCase( int id)
    {
    id_ = id;
    setParams( null);
    }

  /**
   * Returns the id number for this test case.
   */
  public int getId()
    {
    return id_;
    }

  /**
   * Changes the server URI for this request.
   */
  public void setServer( URI uri)
    {
    server_ = uri;
    }

  /**
   * Changes the server URI for this request.
   */
  public void setServer( String uri)
    {
    try
      {
      setServer( new URI( uri));
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Invalid URI=%s", uri), e);
      }
    }

  /**
   * Returns the server URI for this request.
   */
  public URI getServer()
    {
    return server_;
    }

  /**
   * Changes the API version for this request.
   */
  public void setVersion( String version)
    {
    version_ = version;
    }

  /**
   * Returns the API version for this request.
   */
  public String getVersion()
    {
    return version_;
    }

  /**
   * Changes the path for this request.
   */
  public void setPath( String path)
    {
    path_ = path;
    }

  /**
   * Returns the path for this request.
   */
  public String getPath()
    {
    return path_;
    }

  /**
   * Changes the operation for this request.
   */
  public void setOperation( String operation)
    {
    op_ = operation;
    }

  /**
   * Returns the operation for this request.
   */
  public String getOperation()
    {
    return op_;
    }

  /**
   * Changes the parameter definitions for this request.
   */
  public void setParams( Iterable<ParamData> params)
    {
    params_ =
      toStream( Optional.ofNullable( params).orElse( emptyList()))
      .collect( toList());
    }

  /**
   * Returns the parameter definitions for this request.
   */
  public Iterable<ParamData> getParams()
    {
    return params_;
    }

  /**
   * Adds a parameter definition for this request.
   */
  public void addParam( ParamData param)
    {
    params_.add( param);
    }

  /**
   * Changes the definition of the request body.
   */
  public void setBody( MessageData body)
    {
    body_ = body;
    }

  /**
   * Returns the definition of the request body.
   */
  public MessageData getBody()
    {
    return body_;
    }

  /**
   * Changes the description of the invalid input for this request.
   */
  public void setInvalidInput( String invalidInput)
    {
    invalidInput_ = invalidInput;
    }

  /**
   * Returns the description of the invalid input for this request. Returns null for a valid request.
   */
  public String getInvalidInput()
    {
    return invalidInput_;
    }

  /**
   * Returns true for a request with an invalid input.
   */
  public boolean isFailure()
    {
    return getInvalidInput() != null;
    }

  public boolean equals( Object object)
    {
    RequestCase other =
      object instanceof RequestCase
      ? (RequestCase) object
      : null;

    return
      other != null
      && other.getId() == getId();
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ getId();
    }
  
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getId())
      .append( getOperation())
      .append( getPath())
      .append( isFailure()? "FAILURE" : "SUCCESS")
      .toString();
    }

  private final int id_;
  private URI server_;
  private String version_;
  private String path_;
  private String op_;
  private List<ParamData> params_;
  private MessageData body_;
  private String invalidInput_;
  }
