//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter.encoder;

import java.net.URI;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Base class for URI encoders.
 */
public abstract class UriEncoder
  {
  /**
   * Identifies the component of a URI where an encoded value is located
   */
  public enum Component {
    /**
     * Value not located in a URI component
     */
    NONE,
    
    /**
     * Value located in the URI path
     */ 
    PATH,

    /**
     * Value located in the URI query
     */ 
    QUERY
  };
    
  /**
   * Creates a new UriEncoder instance.
   */
  protected UriEncoder( Component component)
    {
    component_ = component;
    }

  /**
   * Returns the URI component to be encoded
   */
  public Component getComponent()
    {
    return component_;
    }

  /**
   * If encoding is enabled, returns the URI-coded form of the given value.
   * Otherwise, returns the value.
   */
  protected String uriEncoded( String value)
    {
    return uriEncoded( getComponent(), value);
    }

  /**
   * Returns the encoded form of the given value as it would appear in the given URI component.
   */
  public static String uriEncoded( Component component, String value)
    {
    String encoded;
    switch( Optional.ofNullable( component).filter( c -> value != null).orElse( Component.NONE))
      {
      case QUERY:
        {
        encoded =
          uriMatcher( null, value).group(2)
          .replaceAll( "\\?", "%3F")
          .replaceAll( "=",   "%3D")
          .replaceAll( "\\&", "%26")
          ;
        break;
        }
      case PATH:
        {
        encoded = uriMatcher( value, null).group(1);
        break;
        }
      case NONE:
        {
        encoded = value;
        break;
        }
      default:
        {
        throw new IllegalArgumentException( String.format( "Unknown component=%s", component));
        }
      }

    return encoded;
    }

  private static Matcher uriMatcher( String path, String query)
    {
    try
      {
      String uri =
        new URI(
          "http",
          null,
          "host",
          -1,
          "/" + Optional.ofNullable( path).orElse( "path"),
          Optional.ofNullable( query).orElse( "query"),
          null)
        .toASCIIString();
        
      Matcher matcher = uriPattern_.matcher( uri);
      if( !matcher.matches())
        {
        throw new IllegalArgumentException( String.format( "Can't recognize uri=%s", uri));
        }
        
      return matcher;
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( String.format( "Can't encode path='%s', query='%s'", path, query), e);
      }
    }

  private final Component component_;
  private static final Pattern uriPattern_ = Pattern.compile( "http://host/([^?]*)\\?(.*)");
  }
