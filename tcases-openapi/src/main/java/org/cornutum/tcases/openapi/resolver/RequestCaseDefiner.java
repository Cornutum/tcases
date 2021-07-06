//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.*;
import org.cornutum.tcases.openapi.Characters;
import org.cornutum.tcases.openapi.OpenApiUtils;
import org.cornutum.tcases.openapi.resolver.ParamDef.Location;

import static org.cornutum.tcases.openapi.resolver.VarProperties.*;
import static org.cornutum.tcases.util.CollectionUtils.toOrderedSet;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toMap;

/**
 * Converts a generic {@link TestCase} for an API request into a {@link RequestCaseDef} that can be used
 * to resolve this abstract test case into an {@link RequestCase executable form}.
 */
public class RequestCaseDefiner
  {
  /**
   * Creates a new RequestCaseDefiner instance.
   */
  public RequestCaseDefiner()
    {
    }

  /**
   * Converts a generic {@link TestCase} for an API request into a {@link RequestCaseDef} that can be used
   * to resolve this abstract test case into an {@link RequestCase executable form}.
   */
  public RequestCaseDef toRequestCaseDef( TestCase testCase) throws RequestCaseException
    {
    try
      {
      final RequestCaseDef requestCaseDef;

      if( testCase == null)
        {
        requestCaseDef = null;
        }
      else
        {
        requestCaseDef = new RequestCaseDef( testCase.getId());
        requestCaseDef.setName( testCase.getName());

        requestCaseDef.setServer(
          Optional.ofNullable( testCase.getAnnotation( "server"))
          .map( uri -> {
            try
              {
              return new URI( uri);
              }
            catch( Exception e)
              {
              throw new RequestCaseException( "Can't convert server URI", e);
              }
            })
          .orElse( null));

        requestCaseDef.setVersion(
          Optional.ofNullable( testCase.getAnnotation( "version"))
          .orElseThrow( () -> new RequestCaseException( "No version annotation defined")));

        requestCaseDef.setApi(
          Optional.ofNullable( testCase.getAnnotation( "title"))
          .orElseThrow( () -> new RequestCaseException( "No title annotation defined")));

        requestCaseDef.setPath(
          Optional.ofNullable( testCase.getAnnotation( "path"))
          .orElseThrow( () -> new RequestCaseException( "No path annotation defined")));

        requestCaseDef.setOperation(
          Optional.ofNullable( testCase.getAnnotation( "operation"))
          .orElseThrow( () -> new RequestCaseException( "No operation annotation defined")));

        requestCaseDef.setInvalidInput(
          Optional.ofNullable( testCase.getInvalidValue())
          .map( binding -> String.format( "%s=%s", binding.getVar(), binding.getValue()))
          .orElse( null));

        requestCaseDef.setAuthFailure(
          Optional.ofNullable( testCase.getInvalidValue())
          .map( binding -> binding.getAnnotation( "authFailure") != null)
          .orElse( false));

        paramProperties( testCase)
          .forEach( (paramName, paramProperties) -> requestCaseDef.addParam( toParamDef( paramName, paramProperties)));

        requestCaseDef.setBody(
          bodyValues( testCase)
          .map( this::toBodyDef)
          .orElse( null));

        authProperties( testCase)
          .forEach( authProperties -> requestCaseDef.addAuthDef( toAuthDef( authProperties)));
        }

      return requestCaseDef;
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't convert test case=%s", testCase.getId()), e);
      }
    }

  /**
   * Returns a mapping of each request parameter defined by the given test case to the
   * set of all parameter properties.
   */
  private Map<String,Map<String,Object>> paramProperties( TestCase testCase)
    {
    return
      toStream( testCase.getVarBindings())
      .filter( binding -> isParamInputType( binding.getType()))
      .collect( groupingBy( this::getInputName))
      .entrySet().stream()
      .collect(
        toMap(
          paramBindings -> getParamName( paramBindings.getValue()),
          paramBindings -> getPropertyValues( paramBindings.getValue())));
    }

  /**
   * Returns if the given input type designates a request parameter.
   */
  private boolean isParamInputType( String inputType)
    {
    return !("request".equals( inputType) || "implicit".equals( inputType) || "security".equals( inputType));
    }

  /**
   * If the given test case specifies a request body, returns the
   * set of all body properties.
   */
  private Optional<Map<String,Object>> bodyValues( TestCase testCase)
    {
    return
      toStream( testCase.getVarBindings())
      .filter( binding -> "request".equals( binding.getType()))
      .collect( groupingBy( this::getInputName))
      .entrySet().stream()
      .findFirst()
      .map( bodyBindings -> getPropertyValues( bodyBindings.getValue()));
    }

  /**
   * Returns the properties of each authentication input defined by the given test case.
   */
  private Stream<Map<String,Object>> authProperties( TestCase testCase)
    {
    Stream.Builder<Map<String,Object>> authProperties = Stream.builder();

    toStream( testCase.getVarBindings())
      .filter( binding -> "security".equals( binding.getType()))
      .collect( groupingBy( this::getInputName))
      .entrySet().stream()
      .findFirst()
      .map( secBindings -> getPropertyValues( secBindings.getValue()))
      .ifPresent( secProperties -> getAuthProperties( secProperties, authProperties));
    
    return authProperties.build();
    }

  /**
   * Returns the properties of each authentication input defined by the given test case.
   */
  private void getAuthProperties( Map<String,Object> secProperties, Stream.Builder<Map<String,Object>> authProperties)
    {
    for( String property : secProperties.keySet())
      {
      Optional<VarBinding> authType =
        Optional.ofNullable( getIfVarBinding( secProperties, property))
        .filter( binding -> "Type".equals( property) && !binding.isValueNA());

      if( authType.isPresent())
        {
        authProperties.add( secProperties);
        }
      else
        {
        Optional.ofNullable( getIfPropertyValues( secProperties, property))
          .ifPresent( memberProperties -> getAuthProperties( memberProperties, authProperties));
        }
      }
    }

  /**
   * Returns the request input name for the given variable binding.
   */
  private String getInputName( VarBinding binding)
    {
    return getPathFirst( getVarPath( binding));
    }

  /**
   * Returns the request parameter name for the given variable bindings.
   */
  private String getParamName( List<VarBinding> bindings)
    {
    VarBinding binding = bindings.get(0);
    return
      Optional.ofNullable( binding.getAnnotation( "paramName"))
      .orElseThrow( () -> new RequestCaseException( String.format( "No parameter name defined for var=%s", getInputName( binding))));
    }

  /**
   * Returns the request parameter definition specified by the given properties.
   */
  private ParamDef toParamDef( String paramName, Map<String,Object> propertyValues)
    {
    ParamDef paramDef = new ParamDef( paramName);

    VarBinding defined = expectVarBinding( propertyValues, "Defined");
    paramDef.setLocation( defined.getType());
    paramDef.setStyle( defined.getAnnotation( "style"));
    paramDef.setExploded( Boolean.parseBoolean( defined.getAnnotation( "explode")));
    paramDef.setValue( toValueDef( defined, null, propertyValues, getParamCharacters( paramDef)));
    
    return paramDef;
    }

  /**
   * Returns the characters allowed in values for the given parameter.
   */
  private Characters getParamCharacters( ParamDef param)
    {
    return OpenApiUtils.getParamCharacters( String.valueOf( param.getLocation()).toLowerCase(), param.getStyle());
    }

  /**
   * Returns the request body definition specified by the given properties.
   */
  private ValueDef<?> toBodyDef( Map<String,Object> bodyValues)
    {
    VarBinding mediaType = expectVarBinding( bodyValues, "Media-Type");
    VarBinding contentDefined = expectVarBinding( bodyValues, "Defined");
    Map<String,Object> contentValues = getPropertyValues( bodyValues, String.valueOf( mediaType.getValue()));
    
    return toValueDef( contentDefined, mediaType, contentValues, Characters.ANY);
    }

  /**
   * Returns the authentication definition specified by the given properties.
   */
  private AuthDef toAuthDef( Map<String,Object> authValues)
    {
    AuthDef authDef;
    
    String type = String.valueOf( expectVarBinding( authValues, "Type").getValue());
    if( "apiKey".equals( type))
      {
      authDef =
        new ApiKeyDef(
          Location.valueOf( String.valueOf( expectVarBinding( authValues, "Location").getValue()).toUpperCase()),
          String.valueOf( expectVarBinding( authValues, "Name").getValue()));
      }
    else if( "http".equals( type))
      {
      String scheme = String.valueOf( expectVarBinding( authValues, "Scheme").getValue());
      if( "basic".equals( scheme))
        {
        authDef = new HttpBasicDef();
        }
      else if( "bearer".equals( scheme))
        {
        authDef = new HttpBearerDef();
        }
      else
        {
        throw new IllegalStateException( String.format( "HTTP authentication scheme=%s is not supported", scheme));
        }
      }
    else
      {
      throw new IllegalStateException( String.format( "Authentication type=%s is not supported", type));
      }
    
    return authDef;
    }

  /**
   * Returns the value definition specified by the given properties.
   */
  private ValueDef<?> toValueDef( VarBinding defined, VarBinding mediaType, Map<String,Object> propertyValues, Characters chars)
    {
    boolean valueDefined =
      Optional.ofNullable( defined)
      .map( binding -> "Yes".equals( binding.getValue()))
      .orElse( true);

    ValueDef<?> valueDef =
      valueDefined
      ? toValueDomain( propertyValues, chars).valueOf()
      : new ValueDef<Object>( null);
      
    valueDef.setValid(
      Stream.concat(
        Optional.ofNullable( defined).map( Stream::of).orElse( Stream.empty()),
        Stream.concat(
          Optional.ofNullable( mediaType).map( Stream::of).orElse( Stream.empty()),
          Optional.ofNullable( propertyValues).map( VarProperties::getVarBindings).orElse( Stream.empty())))
      .allMatch( VarBinding::isValueValid));
        
    Optional<String> mediaTypeSpecified =
      Optional.ofNullable( mediaType)
      .map( binding -> binding.getAnnotation( "mediaType"));

    Optional<Set<String>> mediaTypesExcluded =
      Optional.ofNullable( mediaType)
      .flatMap( binding -> Optional.ofNullable( binding.getAnnotationList( "excluded")))
      .map( excluded -> excluded.stream().collect( toOrderedSet()));    

    valueDef.setMediaType(
      mediaTypesExcluded
      .map( excluded -> (ValueDomain<String>) MediaTypeDomain.except( excluded))
      .orElse( mediaTypeSpecified.map( StringConstant::new).orElse( null)));
      
    return valueDef;
    }

  }
