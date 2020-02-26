//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Optional;
import java.util.Random;
import static java.util.stream.Collectors.toList;

/**
 * Resolves an {@link RequestCaseDef abstract test case} for an API request by producing a {@link RequestCase}
 * that describes an executable instance of this test case.
 */
public class RequestCaseResolver
  {
  /**
   * Creates a new RequestCaseResolver instance.
   */
  public RequestCaseResolver( Random random)
    {
    random_ = random;
    }

  /**
   * Resolves an {@link RequestCaseDef abstract test case} for an API request by producing a {@link RequestCase}
   * that describes an executable instance of this test case.
   */
  public RequestCase resolve( RequestCaseDef requestCaseDef)
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

    requestCase.setBody(
      Optional.ofNullable( requestCaseDef.getBody())
      .map( this::resolveMessageData)
      .orElse( null));
    
    return requestCase;
    }

  /**
   * Returns a resolved request parameter data object.
   */
  private ParamData resolveParamData( ParamDef paramDef)
    {
    ParamData paramData = new ParamData( paramDef.getName(), resolveMessageData( paramDef.getValue()));

    paramData.setLocation( paramDef.getLocation());
    paramData.setStyle( paramDef.getStyle());
    paramData.setExploded( paramDef.isExploded());

    return paramData;
    }

  /**
   * Returns a resolved request data object.
   */
  private MessageData resolveMessageData( ValueDef<?> valueDef)
    {
    DataValue<?> resolvedValue = 
      valueDef.isDefined()
      ? valueDef.getDomain().select( getRandom())
      : null;

    String resolvedMediaType =
      Optional.ofNullable( valueDef.getMediaType())
      .map( mediaType -> mediaType.selectValue( getRandom()))
      .orElse( null);
    
    return new MessageData( resolvedValue, resolvedMediaType, valueDef.isValid());
    }

  /**
   * Returns the random number generator used to resolve request input data.
   */
  public Random getRandom()
    {
    return random_;
    }

  private final Random random_;
  }
