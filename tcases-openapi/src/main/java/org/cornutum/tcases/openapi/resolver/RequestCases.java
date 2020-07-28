//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.FunctionTestDef;
import org.cornutum.tcases.SystemTestDef;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static java.util.stream.Collectors.toList;

/**
 * Defines methods for generating {@link RequestCase request test cases} from the {@link SystemTestDef test definitions} for
 * requests defined by an OpenAPI specification.
 */
public final class RequestCases
  {
  /**
   * Creates a new RequestCases instance.
   */
  private RequestCases()
    {
    // Static methods only
    }

  /**
   * Reduces the given request test definition to those request cases that can be realized by an
   * executable test. This eliminates request cases that become redundant or infeasible when input
   * data is serialized into HTTP messages.
   */
  public static RequestTestDef realizeRequestCases( RequestTestDef requestTestDef)
    {
    List<RequestCase> serializable =
      requestTestDef.getRequestCases().stream()
      .filter( RequestCases::isSerializable)
      .collect( toList());

    List<RequestCase> dups = getSerializedDups( serializable);
    dups.stream().forEach( rc -> serializable.remove( rc));

    return new RequestTestDef( serializable);
    }

  /**
   * Returns the request cases resolved for the given system test definition.
   */
  public static RequestTestDef getRequestCases( SystemTestDef testDef, ResolverContext context)
    {
    RequestCaseResolver resolver = new RequestCaseResolver( context);

    return
      new RequestTestDef(
        getRequestCaseDefs( testDef).stream()
        .map( requestCaseDef -> resolver.resolve( requestCaseDef))
        .filter( Objects::nonNull)
        .collect( toList()));
    }

  /**
   * Returns the request case definitions for the given system test definition.
   */
  public static List<RequestCaseDef> getRequestCaseDefs( SystemTestDef testDef)
    {
    return getRequestCaseDefs( new RequestCaseDefiner(), testDef);
    }

  /**
   * Returns the request case definitions for the given system test definition.
   */
  private static List<RequestCaseDef> getRequestCaseDefs( RequestCaseDefiner definer, SystemTestDef testDef)
    {
    return
      toStream( testDef.getFunctionTestDefs())
      .flatMap( function -> getRequestCaseDefs( definer, function).stream())
      .collect( toList());
    }

  /**
   * Returns the request case definitions for the given function test definition.
   */
  private static List<RequestCaseDef> getRequestCaseDefs( RequestCaseDefiner definer, FunctionTestDef testDef)
    {
    return
      toStream( testDef.getTestCases())
      .map( testCase -> {
        try
          {
          return definer.toRequestCaseDef( testCase);
          }
        catch( Exception e)
          {
          throw new RequestCaseException( String.format( "Can't get request case for function=%s, test case=%s", testDef.getName(), testCase.getId()), e);
          }
        })
      .collect( toList());
    }

  /**
   * Returns true if the given request case is feasible when input data is serialized into an HTTP message.
   */
  private static boolean isSerializable( RequestCase requestCase)
    {
    // A test case for a failure to get a value of type="string" is generally not feasible when all
    // input is serialized to message strings...
    boolean stringTypeFailure =
      Optional.ofNullable( requestCase.getInvalidInput())
      .map( invalidInput -> invalidInput.endsWith( "Type=Not string"))
      .orElse( false);

    // ...unless the input is a JSON-encoded request body
    boolean invalidBodyJson =
      Optional.ofNullable( stringTypeFailure? requestCase.getBody() : null)
      .map( body -> !body.isValid() && "application/json".equals( body.getMediaType()))
      .orElse( false);

    return !stringTypeFailure || invalidBodyJson;
    }

  /**
   * Returns the subset of the given request cases that are duplicates of other
   * request cases after serialization.
   */
  private static List<RequestCase> getSerializedDups( List<RequestCase> requestCases)
    {
    List<String> nullFailureInputs =
      requestCases.stream()
      .map( RequestCases::nullFailureInput)
      .filter( Objects::nonNull)
      .collect( toList());

    return
      nullFailureInputs.stream()
      .flatMap( id -> requestCases.stream().filter( rc -> isStringEmptyFailureDup( rc, id) || isUndefinedFailureDup( rc, id)))
      .collect( toList());
    }

  /**
   * If the given request case represents an unexpected null value failure, returns the id of the failure input.
   * Otherwise, returns null.
   */
  private static String nullFailureInput( RequestCase requestCase)
    {
    return
      Optional.ofNullable( requestCase.getInvalidInput())
      .map( invalid -> invalid.lastIndexOf( ".Type=null"))
      .filter( end -> end >= 0)
      .map( end -> requestCase.getInvalidInput().substring( 0, end))
      .orElse( null);
    }

  /**
   * Returns if the given request case represents a failure caused by an empty string that
   * duplicates a null value when serialized.
   */
  private static boolean isStringEmptyFailureDup( RequestCase requestCase, String inputId)
    {
    return
      getFailureData( requestCase)
      .filter( failure -> requestCase.getInvalidInput().equals( String.format( "%s.Value.Length=0", inputId)))
      .map( failure -> failure instanceof ParamData || !"application/json".equals( failure.getMediaType()))
      .orElse( false);
    }

  /**
   * Returns if the given request case represents a failure caused by an undefined value that
   * duplicates a null value when serialized.
   */
  private static boolean isUndefinedFailureDup( RequestCase requestCase, String inputId)
    {
    return
      getFailureData( requestCase)
      .filter( failure -> requestCase.getInvalidInput().equals( String.format( "%s.Defined=No", inputId)))
      .map( failure -> failure instanceof ParamData && "simple".equals( ((ParamData) failure).getStyle()))
      .orElse( false);
    }

  /**
   * If the given request case is a failure case, return the data for the invalid input.
   * Otherwise, returns null;
   */
  private static Optional<MessageData> getFailureData( RequestCase requestCase)
    {
    return
      Optional.of( requestCase)
      .filter( RequestCase::isFailure)
      .map( rc -> {
        return
          toStream( rc.getParams())
          .filter( param -> !param.isValid())
          .findFirst()
          .map( param -> (MessageData) param)
          .orElse( rc.getBody());
        });
    }

  }
