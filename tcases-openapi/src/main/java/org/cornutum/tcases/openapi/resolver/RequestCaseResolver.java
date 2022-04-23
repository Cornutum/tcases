//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.ConditionReporter;
import org.cornutum.tcases.openapi.InvalidStyleException;
import org.cornutum.tcases.openapi.OpenApiUtils;
import org.cornutum.tcases.openapi.resolver.ParamDef.Location;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Resolves an {@link RequestCaseDef abstract test case} for an API request by producing a {@link RequestCase}
 * that describes an executable instance of this test case.
 */
public class RequestCaseResolver extends ConditionReporter<ResolverContext>
  {
  /**
   * Creates a new RequestCaseResolver instance.
   */
  public RequestCaseResolver( ResolverContext context)
    {
    super( context);
    setNotifier( context.getNotifier());
    }

  /**
   * Resolves an {@link RequestCaseDef abstract test case} for an API request by producing a {@link RequestCase}
   * that describes an executable instance of this test case.
   */
  public RequestCase resolve( RequestCaseDef requestCaseDef)
    {
    return resultFor( String.valueOf( requestCaseDef), () -> {
      try
        {
        RequestCase requestCase = new RequestCase( requestCaseDef.getId());

        requestCase.setName( requestCaseDef.getName());
        requestCase.setServer( requestCaseDef.getServer());
        requestCase.setVersion( requestCaseDef.getVersion());
        requestCase.setApi( requestCaseDef.getApi());
        requestCase.setPath( requestCaseDef.getPath());
        requestCase.setOperation( requestCaseDef.getOperation());
        requestCase.setInvalidInput( requestCaseDef.getInvalidInput());

        requestCase.setAuthDefs( requestCaseDef.getAuthDefs());
        requestCase.setAuthFailure( requestCaseDef.isAuthFailure());

        requestCase.setParams(
          toStream( requestCaseDef.getParams())
          .map( paramDef -> resolveParamData( paramDef))
          .collect( toList()));

        requestCase.setBody( resolveBody( requestCaseDef.getBody()));
    
        return requestCase;
        }
      catch( ResolverSkipException skip)
        {
        getNotifier().error( skip.getLocation(), skip.getMessage(), "No request case created for this test");
        return null;
        }
      catch( Exception e)
        {
        throw new ResolverException( String.format( "Can't resolve %s", requestCaseDef), e);
        }
      });
    }

  /**
   * Returns a resolved request parameter data object.
   */
  private ParamData resolveParamData( ParamDef paramDef)
    {
    return resultFor( paramDef.getName(), () -> { 
      try
        {
        ParamData paramData = new ParamData( paramDef.getName(), resolveMessageData( paramDef.getValue()));

        paramData.setLocation( paramDef.getLocation());
        paramData.setStyle( getApplicableStyle( paramDef.getStyle(), paramDef.getLocation(), paramData.getType()));
        paramData.setExploded( paramDef.isExploded());

        return paramData;
        }
      catch( ResolverSkipException skip)
        {
        throw skip;
        }
      catch( Exception e)
        {
        throw new ResolverException( String.format( "Can't resolve parameter=%s", paramDef.getName()), e);
        }
      });
    }

  /**
   * Returns a resolved request header data object.
   */
  private HeaderData resolveHeaderData( HeaderDef headerDef)
    {
    return resultFor( headerDef.getName(), () -> { 
      try
        {
        HeaderData headerData = new HeaderData( headerDef.getName(), resolveMessageData( headerDef.getValue()));
        headerData.setExploded( headerDef.isExploded());

        return headerData;
        }
      catch( ResolverSkipException skip)
        {
        throw skip;
        }
      catch( Exception e)
        {
        throw new ResolverException( String.format( "Can't resolve header=%s", headerDef.getName()), e);
        }
      });
    }

  /**
   * Returns the validated style attribute for the given parameter. When the specified style
   * is not applicable for this parameter, returns a default applicable style.
   */
  private String getApplicableStyle( String style, Location location, DataValue.Type type)
    {
    try
      {
      return
        OpenApiUtils.ifApplicableStyle(
          style,
          String.valueOf( location).toLowerCase(),
          String.valueOf( type).toLowerCase());
      }
    catch( InvalidStyleException e)
      {
      String applicableStyle = e.getValidStyle();
      notifyWarning( String.format( "%s -- using style=%s instead", e.getMessage(), applicableStyle));
      return applicableStyle;
      }
    }

  /**
   * Returns a resolved request body data object.
   */
  private MessageData resolveBody( ValueDef<?> body)
    {
    return resultFor( "requestBody", () -> {
      try
        {
        return
          Optional.ofNullable( body)
          .map( this::resolveMessageData)
          .orElse( null);
        }
      catch( ResolverSkipException skip)
        {
        throw skip;
        }
      catch( Exception e)
        {
        throw new ResolverException( "Can't resolve request body", e);
        }
      });
    }

  /**
   * Returns a resolved request data object.
   */
  private MessageData resolveMessageData( ValueDef<?> valueDef)
    {
    DataValue<?> resolvedValue = 
      valueDef.isDefined()
      ? resultFor( "value", () -> domainValue( valueDef.getDomain()))
      : null;

    String resolvedMediaType =
      Optional.ofNullable( valueDef.getMediaType())
      .map( mediaType -> resultFor( "mediaType", () -> domainValue( mediaType).getValue()))
      .orElse( null);
    
    MessageData messageData = new MessageData( resolvedValue, resolvedMediaType, valueDef.isValid());

    Optional.ofNullable( resolvedValue)
      .map( DataValue::getType)
      .filter( type -> DataValue.Type.OBJECT.equals( type))
      .ifPresent( type -> { 
        messageData.setEncodings(
          "application/x-www-form-urlencoded".equals( resolvedMediaType)?
          resolveUrlEncodedFormEncodings( (ObjectValue) resolvedValue, valueDef.getEncodings()) :

          "multipart/form-data".equals( resolvedMediaType)?
          resolveMultipartFormEncodings( (ObjectValue) resolvedValue, valueDef.getEncodings()) :
      
          null);
        });

    return messageData;
    }

  /**
   * Returns resolved encodings for the parts of an <CODE>application/x-www-form-urlencoded</CODE> data value.
   */
  private Map<String,EncodingData> resolveUrlEncodedFormEncodings( ObjectValue formValue, Map<String,EncodingDef> encodings)
    {
    return
      formValue.getValue().keySet().stream()
      .collect(
        toMap(
          property -> property,

          property -> {
            EncodingDef encoding = encodings.get( property);

            String style =
              Optional.ofNullable( encoding)
              .flatMap( def -> Optional.ofNullable( def.getStyle()))
              .orElse( "form");
            
            Boolean exploded =
              Optional.ofNullable( encoding)
              .flatMap( def -> Optional.ofNullable( encoding.isExploded()))
              .orElse( "form".equals( style));
            
            return new EncodingData( style, exploded);
          },

          (v1,v2) -> v1,
          LinkedHashMap::new));
    }

  /**
   * Returns resolved encodings for the parts of an <CODE>multipart/form-data</CODE> data value.
   */
  private Map<String,EncodingData> resolveMultipartFormEncodings( ObjectValue formValue, Map<String,EncodingDef> encodings)
    {
    return
      formValue.getValue().keySet().stream()
      .collect(
        toMap(
          property -> property,

          property -> {
            EncodingDef encoding = encodings.get( property);

            String contentType =
              Optional.ofNullable( encoding)
              .flatMap( def -> Optional.ofNullable( encoding.getContentType()))
              .orElse( defaultContentType( formValue.getValue().get( property)));

            List<HeaderData> headers =
              Optional.ofNullable( encoding)
              .map( EncodingDef::getHeaders)
              .orElse( emptyList())
              .stream()
              .map( this::resolveHeaderData)
              .collect( toList());

            return new EncodingData( contentType, headers);
          },

          (v1,v2) -> v1,
          LinkedHashMap::new));
    }

  /**
   * Returns the default content type for the given <CODE>multipart/form-data</CODE> part value.
   */
  private String defaultContentType( DataValue<?> partValue)
    {
    String contentType;
    switch( partValue.getType())
      {
      case OBJECT:
        {
        contentType = "application/json";
        break;
        }

      case STRING:
        {
        contentType =
          Optional.ofNullable( partValue.getFormat())
          .filter( format -> "base64".equals( format)  || "binary".equals( format) )
          .map( format -> "application/octet-stream")
          .orElse( "text/plain");
        break;
        }

      case ARRAY:
        {
        contentType =
          ((ArrayValue<?>) partValue).getValue()
          .stream()
          .findFirst()
          .map( this::defaultContentType)
          .orElse( "text/plain");
        break;
        }

      default:
        {
        contentType = "text/plain";
        break;
        }
      }
      
    return contentType;
    }

  /**
   * Returns a random data value from the given {@link ValueDomain}.
   */
  private <T> DataValue<T> domainValue( ValueDomain<T> domain)
    {
    try
      {
      return domain.select( getContext());
      }
    catch( ResolverSkipException skip)
      {
      throw skip;
      }
    catch( Exception e)
      {
      throw new ResolverException( String.format( "Can't get value from %s", domain), e);
      }
    }
  }
