//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.ApiKeyDef;
import org.cornutum.tcases.openapi.resolver.AuthDefVisitor;
import org.cornutum.tcases.openapi.resolver.HttpBasicDef;
import org.cornutum.tcases.openapi.resolver.HttpBearerDef;
import org.cornutum.tcases.openapi.resolver.MessageData;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.test.MediaRange;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueConverter;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueJson;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueText;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Base class for TestCaseWriter implementations.
 *
 */
public abstract class BaseTestCaseWriter implements TestCaseWriter
  {
  /**
   * Creates a new BaseTestCaseWriter instance.
   */
  protected BaseTestCaseWriter()
    {
    setValidateResponses( true);
    setTrustServer( false);
    getDefaultConverters().forEach( (mediaType,converter) -> setConverter( mediaType, converter));
    }
  
  /**
   * Changes if generated test cases will validate API requests responses.
   */
  public void setValidateResponses( boolean validateResponses)
    {
    validateResponses_ = validateResponses;
    }

  /**
   * Returns if generated test cases will validate API requests responses.
   */
  public boolean validateResponses()
    {
    return validateResponses_;
    }
  
  /**
   * Changes if generated HTTPS requests will accept an untrusted API server.
   */
  public void setTrustServer( boolean trustServer)
    {
    trustServer_ = trustServer;
    }

  /**
   * Returns if generated HTTPS requests will accept an untrusted API server.
   */
  public boolean trustServer()
    {
    return trustServer_;
    }

  /**
   * Returns the test case dependencies used by this writer.
   */
  public Depends getDepends()
    {
    return depends_;
    }
  
  /**
   * Prepare this writer to handle the given request cases.
   */
  @Override
  public void prepareTestCases( List<RequestCase> requestCases)
    {
    depends_ = new Depends();

    depends_.setValidateResponses( validateResponses());
    depends_.setTrustServer( trustServer());

    AuthDependsVisitor authDependsVisitor = new AuthDependsVisitor();
    requestCases.stream()
      .flatMap( rc -> toStream( rc.getAuthDefs()))
      .forEach( authDef -> authDef.accept( authDependsVisitor));

    requestCases.stream()
      .filter( rc -> rc.isFailure() && !rc.isAuthFailure())
      .findFirst()
      .ifPresent( rc -> getDepends().setDependsFailure());

    requestCases.stream()
      .filter( RequestCase::isAuthFailure)
      .findFirst()
      .ifPresent( rc -> getDepends().setDependsAuthFailure());

    requestCases.stream()
      .map( rc -> Optional.ofNullable( rc.getBody()).map( MessageData::getMediaType).orElse( null))
      .filter( mediaType -> mediaType != null && "multipart/form-data".equals( MediaRange.of( mediaType).base()))
      .findFirst()
      .ifPresent( multipart -> getDepends().setDependsMultipart());
    }

  /**
   * Returns the default {@link #getConverter(String) converters} for this test case writer.
   */
  protected Map<String,DataValueConverter<String>> getDefaultConverters()
    {
    DataValueConverter<String> textPlain = new DataValueText();

    return
      new MapBuilder<String,DataValueConverter<String>>()
      .put( "*/*", textPlain)
      .put( "text/*", textPlain)
      .put( "text/plain", textPlain)
      .put( "application/json", new DataValueJson())
      .put( "application/*+json", new DataValueJson())
      .build();
    }
  
  /**
   * Changes the serializer used for the given media type(s). The <CODE>mediaType</CODE> can be an valid
   * <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media-range</a>, including wild cards.
   */
  public void setConverter( String mediaType, DataValueConverter<String> converter)
    {
    if( converter == null)
      {
      converters_.remove( validMediaType( mediaType));
      }
    else
      {
      converters_.put( validMediaType( mediaType), converter);
      }
    }

  /**
   * Returns the serializer used for the given media type. The <CODE>mediaRange</CODE> must be a specific
   * <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media-range</a> with no wild cards.
   * Returns the serializer registered for the media type that most closely matches the given media range.
   */
  public Optional<DataValueConverter<String>> getConverter( MediaRange mediaRange)
    {
    Object[] alternatives = new Object[]{
      mediaRange.toString(),
      mediaRange.baseStructured(),
      MediaRange.anyOf( mediaRange.type(), mediaRange.suffix()),
      mediaRange.base(),
      MediaRange.anyOf( mediaRange.type()),
      MediaRange.any()};

    return
      Arrays.stream( alternatives)
      .map( String::valueOf)
      .filter( alternative -> converters_.containsKey( alternative))
      .map( alternative -> converters_.get( alternative))
      .findFirst();
    }

  /**
   * Returns the serializer used for the given media type. The <CODE>mediaType</CODE> must be a specific
   * <a href="https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html">media-range</a> with no wild cards.
   * Returns the serializer registered for the media type that most closely matches the given media type.
   */
  public Optional<DataValueConverter<String>> getConverter( String mediaType)
    {
    return getConverter( MediaRange.of( mediaType));
    }
  
  /**
   * Return the valid media type represented by the given string.
   */
  private String validMediaType( String mediaType)
    {
    return
      mediaType == null
      ? "*/*"
      : MediaRange.of( mediaType).toString();
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .build();
    }

  private final Map<String,DataValueConverter<String>> converters_ = new HashMap<String,DataValueConverter<String>>();
  private boolean validateResponses_;
  private boolean trustServer_;
  private Depends depends_;

  /**
   * Defines the test case dependencies for this writer.
   */
  public static class Depends
    {
    /**
     * Creates a new Depends instance.
     */
    public Depends()
      {
      setValidateResponses( true);
      setTrustServer( false);
      }
  
    /**
     * Changes if generated test cases will validate API requests responses.
     */
    public void setValidateResponses( boolean validateResponses)
      {
      validateResponses_ = validateResponses;
      }

    /**
     * Returns if generated test cases will validate API requests responses.
     */
    public boolean validateResponses()
      {
      return validateResponses_;
      }
  
    /**
     * Changes if generated HTTPS requests will accept an untrusted API server.
     */
    public void setTrustServer( boolean trustServer)
      {
      trustServer_ = trustServer;
      }

    /**
     * Returns if generated HTTPS requests will accept an untrusted API server.
     */
    public boolean trustServer()
      {
      return trustServer_;
      }

    /**
     * Registers a dependency on a runtime API server definition.
     */
    public void setDependsServer()
      {
      dependsServer_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime API server definition.
     */
    public boolean dependsServer()
      {
      return dependsServer_;
      }
    
    /**
     * Returns if there is a dependency on a runtime API authentication.
     */
    public boolean dependsAuth()
      {
      return dependsAuth_;
      }
    
    /**
     * Returns if there is a dependency on a runtime HTTP authentication.
     */
    public boolean dependsAuthHttp()
      {
      return dependsAuthHttp_;
      }
    
    /**
     * Registers a dependency on a runtime API key credential.
     */
    public void setDependsApiKey()
      {
      dependsAuth_ = true;
      dependsApiKey_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime API key credential.
     */
    public boolean dependsApiKey()
      {
      return dependsApiKey_;
      }
    
    /**
     * Registers a dependency on a runtime HTTP Basic credential.
     */
    public void setDependsHttpBasic()
      {
      dependsAuth_ = true;
      dependsAuthHttp_ = true;
      dependsHttpBasic_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime HTTP Basic credential.
     */
    public boolean dependsHttpBasic()
      {
      return dependsHttpBasic_;
      }

    /**
     * Registers a dependency on a runtime HTTP Bearer credential.
     */
    public void setDependsHttpBearer()
      {
      dependsAuth_ = true;
      dependsAuthHttp_ = true;
      dependsHttpBearer_ = true;
      }
    
    /**
     * Returns if there is a dependency on a runtime HTTP Bearer credential.
     */
    public boolean dependsHttpBearer()
      {
      return dependsHttpBearer_;
      }

    /**
     * Registers a dependency on test case failures.
     */
    public void setDependsFailure()
      {
      dependsFailure_ = true;
      }

    /**
     * Returns if there is a dependency on test case failures.
     */
    public boolean dependsFailure()
      {
      return dependsFailure_;
      }

    /**
     * Registers a dependency on authorization failures.
     */
    public void setDependsAuthFailure()
      {
      dependsAuthFailure_ = true;
      }

    /**
     * Returns if there is a dependency on authorization failures.
     */
    public boolean dependsAuthFailure()
      {
      return dependsAuthFailure_;
      }

    /**
     * Registers a dependency on the "multipart/form-data" media type.
     */
    public void setDependsMultipart()
      {
      dependsMultipart_ = true;
      }
    
    /**
     * Returns if there is a dependency on the "multipart/form-data" media type.
     */
    public boolean dependsMultipart()
      {
      return dependsMultipart_;
      }

    private boolean dependsApiKey_;
    private boolean dependsAuthFailure_;
    private boolean dependsAuthHttp_;
    private boolean dependsAuth_;
    private boolean dependsFailure_;
    private boolean dependsHttpBasic_;
    private boolean dependsHttpBearer_;
    private boolean dependsMultipart_;
    private boolean dependsServer_;
    private boolean validateResponses_;
    private boolean trustServer_;
    }

  /**
   * Records dependencies for an authentication definition.
   */
  private class AuthDependsVisitor implements AuthDefVisitor
    {
    @Override
    public void visit( ApiKeyDef authDef)
      {
      getDepends().setDependsApiKey();
      }

    @Override
    public void visit( HttpBasicDef authDef)
      {
      getDepends().setDependsHttpBasic();
      }

    @Override
    public void visit( HttpBearerDef authDef)
      {
      getDepends().setDependsHttpBearer();
      }
    }
  }
