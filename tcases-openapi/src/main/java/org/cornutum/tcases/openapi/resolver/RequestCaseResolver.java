//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Optional;

import org.cornutum.tcases.openapi.ConditionReporter;

import static java.util.stream.Collectors.toList;

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
        RequestCase requestCase = new RequestCase();

        requestCase.setServer( requestCaseDef.getServer());
        requestCase.setVersion( requestCaseDef.getVersion());
        requestCase.setPath( requestCaseDef.getPath());
        requestCase.setOperation( requestCaseDef.getOperation());
        requestCase.setInvalidInput( requestCaseDef.getInvalidInput());

        requestCase.setParams(
          toStream( requestCaseDef.getParams())
          .map( paramDef -> resolveParamData( paramDef))
          .collect( toList()));

        requestCase.setBody( resolveBody( requestCaseDef.getBody()));
    
        return requestCase;
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
        paramData.setStyle( paramDef.getStyle());
        paramData.setExploded( paramDef.isExploded());

        return paramData;
        }
      catch( Exception e)
        {
        throw new ResolverException( String.format( "Can't resolve parameter=%s", paramDef.getName()), e);
        }
      });
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
    
    return new MessageData( resolvedValue, resolvedMediaType, valueDef.isValid());
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
    catch( Exception e)
      {
      throw new ResolverException( String.format( "Can't get value from %s", domain), e);
      }
    }
  }
