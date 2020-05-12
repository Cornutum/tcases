//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
import org.cornutum.tcases.openapi.OpenApiUtils;
import org.cornutum.tcases.util.ToString;

import java.net.URI;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.IntStream;

/**
 * Describes an executable test case for an API request.
 */
public class RequestCase implements Comparable<RequestCase>
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
   * Changes the name of this test case.
   */
  public void setName( String name)
    {
    name_ = name;
    }

  /**
   * Returns the name of this test case.
   */
  public String getName()
    {
    return name_;
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
   * Changes the name of the API that defines this request.
   */
  public void setApi( String api)
    {
    api_ = api;
    }

  /**
   * Returns the name of the API that defines this request.
   */
  public String getApi()
    {
    return api_;
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
    params_ = new ArrayList<ParamData>();
    if( params != null)
      {
      for( ParamData param : params)
        {
        addParam( param);
        }
      }
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
    params_.add( DataValueChars.allowed( param));
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

  public int compareTo( RequestCase other)
    {
    return
      Comparator.comparing( RequestCase::getApi)
      .thenComparing( RequestCase::getPath)
      .thenComparing( RequestCase::getOperation)
      .thenComparingInt( RequestCase::getId)
      .compare( this, other);
    }
  
  public boolean equals( Object object)
    {
    RequestCase other =
      object instanceof RequestCase
      ? (RequestCase) object
      : null;

    return
      other != null
      && other.getId() == getId()
      && Objects.equals( other.getApi(), getApi())
      && Objects.equals( other.getPath(), getPath())
      && Objects.equals( other.getOperation(), getOperation())
      ;
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ getId()
      ^ Objects.hashCode( getApi())
      ^ Objects.hashCode( getPath())
      ^ Objects.hashCode( getOperation())
      ;
    }
  
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getId())
      .append( getName())
      .append( getOperation())
      .append( getPath())
      .append( isFailure()? "FAILURE" : "SUCCESS")
      .toString();
    }

  private final int id_;
  private String name_;
  private URI server_;
  private String version_;
  private String api_;
  private String path_;
  private String op_;
  private List<ParamData> params_;
  private MessageData body_;
  private String invalidInput_;
  

  /**
   * Verifies consistency of {@link ParamData} with a set of {@link Characters allowed characters}.
   */
  private static class DataValueChars extends RequestCaseContext implements DataValueVisitor
    {
    /**
     * Returns the given {@link ParamData} if it is consistent with the {@link Characters allowed characters} for this
     * parameter. Otherwise, throws a RequestCaseException.
     */
    public static ParamData allowed( ParamData param)
      {
      return new DataValueChars( param).allowed();
      }

    /**
     * Creates a new DataValueChars instance.
     */
    private DataValueChars( ParamData param)
      {
      param_ = param;
      chars_ = getParamCharacters( param);
      }

    /**
     * Returns the characters allowed in values for the given parameter.
     */
    private Characters getParamCharacters( ParamData param)
      {
      return OpenApiUtils.getParamCharacters( String.valueOf( param.getLocation()).toLowerCase(), param.getStyle());
      }
    
    /**
     * Returns this {@link ParamData} if it is consistent with the required {@link Characters}.
     * Otherwise, throws a RequestCaseException.
     *
     */
    private ParamData allowed()
      {
      DataValue<?> value = param_.getValue();
      if( value != null)
        {
        doFor( String.valueOf( param_), () -> value.accept( this));
        }

      return param_;
      }

    public void visit( ArrayValue<?> data)
      {
      IntStream.range( 0, data.getValue().size())
        .forEach( i -> doFor( String.format( "item[%s]", i), () -> data.getValue().get(i).accept( this)));
      }

    public void visit( BinaryValue data)
      {
      // Non-text value
      }

    public void visit( BooleanValue data)
      {
      // Non-text value
      }

    public void visit( DecimalValue data)
      {
      // Non-text value
      }

    public void visit( IntegerValue data)
      {
      // Non-text value
      }

    public void visit( LongValue data)
      {
      // Non-text value
      }

    public void visit( NullValue data)
      {
      // Non-text value
      }

    public void visit( ObjectValue data)
      {
      doFor( "properties", () -> {
        data.getValue()
          .forEach( (name, value) -> {
            if( !chars_.allowed( name))
              {
              throw new RequestCaseException( String.format( "Property name='%s' is not allowed by %s", name, chars_));
              }
            doFor( name, () -> value.accept( this));
            });
        });
      }
    
    public void visit( StringValue data)
      {
      if( !chars_.allowed( data.getValue()))
        {
        throw new RequestCaseException( String.format( "Value='%s' is not allowed by %s", data.getValue(), chars_));
        }
      }

    private final ParamData param_;
    private final Characters chars_;
    }
  }
