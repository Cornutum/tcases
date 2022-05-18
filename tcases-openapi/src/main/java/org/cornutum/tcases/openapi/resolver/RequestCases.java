//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.FunctionTestDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.openapi.resolver.ParamDef.Location;
import org.cornutum.tcases.openapi.test.MediaRange;

import static org.cornutum.tcases.DefUtils.toIdentifier;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Defines methods for generating {@link RequestCase request test cases} from the {@link SystemTestDef test definitions} for
 * requests defined by an OpenAPI definition.
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
    RequestTestDef realizable =
      new RequestTestDef(
        requestTestDef.getRequestCases().stream()
        .filter( RequestCases::isSerializable)
        .collect( toList()));

    getSerializedDups( realizable)
      .forEach( rc -> realizable.remove( rc));

    return realizable;
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
  private static List<RequestCase> getSerializedDups( RequestTestDef requestTestDef)
    {
    List<RequestCase> dups = new ArrayList<RequestCase>();
    for( String path : requestTestDef.getPaths())
      {
      for( String op : requestTestDef.getOperations( path))
        {
        dups.addAll( getSerializedDups( requestTestDef.getRequestCases( path, op)));
        }
      }
    return dups;
    }

  /**
   * Returns the subset of the given request cases that are duplicates of other
   * request cases after serialization.
   */
  private static List<RequestCase> getSerializedDups( List<RequestCase> requestCases)
    {
    List<RequestCase> nullFailureCases =
      requestCases.stream()
      .filter( RequestCases::isNullFailure)
      .collect( toList());

    Map<String,DataValue.Type> nullFailureTypes =
      nullFailureCases.stream()
      .collect(
        toMap(
          rc -> nullFailureId( rc),
          rc -> nullFailureType( requestCases, rc)));

    return
      nullFailureTypes.keySet().stream()
      .flatMap( id -> {
        DataValue.Type idType = nullFailureTypes.get(id);
        return
          requestCases.stream()
          .filter( rc -> isParamFailureDup( rc, id, idType) || isBodyFailureDup( rc, id, idType));
        })
      .collect( toList());
    }

  /**
   * Returns true if the given request case represents an unexpected null value failure.
   */
  private static boolean isNullFailure( RequestCase requestCase)
    {
    return
      Optional.ofNullable( requestCase.getInvalidInput())
      .map( invalid -> invalid.endsWith( ".Type=null"))
      .orElse( false);
    }

  /**
   * For a null failure case, returns the id of the failure input.
   */
  private static String nullFailureId( RequestCase nullFailure)
    {
    String invalidInput = nullFailure.getInvalidInput();
    return invalidInput.substring( 0, invalidInput.lastIndexOf( ".Type=null"));
    }

  /**
   * For a null failure case, returns the type of the failure input.
   */
  private static DataValue.Type nullFailureType( List<RequestCase> requestCases, RequestCase nullFailure)
    {
    Optional<String> nullParam =
      getParamFailureData( nullFailure)
      .map( ParamData::getName);

    return
      requestCases.stream()
      .map( rc -> {
        return
          nullParam
          .flatMap( name -> toStream( rc.getParams()).filter( pd -> pd.getName().equals( name)).findFirst().map( pd -> (MessageData) pd))
          .orElse( rc.getBody());
        })
      .filter( MessageData::isValid)
      .findFirst()
      .map( MessageData::getType)
      .orElseThrow( () -> new IllegalStateException( String.format( "Can't find null failure type for %s", nullFailure)));
    }

  /**
   * Returns if the given request case represents a failure caused by a parameter value that
   * duplicates a null value when serialized.
   */
  private static boolean isParamFailureDup( RequestCase requestCase, String inputId, DataValue.Type inputType)
    {
    return
      getParamFailureData( requestCase)
      .map( param -> {
        return
          isParamObjectEmptyFailureDup( requestCase, param, inputId)
          || isParamArrayEmptyFailureDup( requestCase, param, inputId)
          || isParamStringEmptyFailureDup( requestCase, param, inputId)
          || isParamUndefinedFailureDup( requestCase, param, inputId, inputType);
        })
      .orElse( false);
    }

  /**
   * Returns if the given request case represents a parameter failure caused by an empty object that
   * duplicates a null value when serialized.
   */
  private static boolean isParamObjectEmptyFailureDup( RequestCase requestCase, ParamData param, String inputId)
    {
    return
      // True for any invalid empty object value...
      requestCase.getInvalidInput().startsWith( String.format( "%s.Value.Property-Count=<", inputId))
      && ((ObjectValue) param.getValue()).getValue().isEmpty()

      // ... except for the value of a form-encoded object parameter
      && !(inputId.equals( toIdentifier( param.getName()))
           && (param.getLocation() == Location.QUERY || param.getLocation() == Location.COOKIE));
    }

  /**
   * Returns if the given request case represents a parameter failure caused by an empty array that
   * duplicates a null value when serialized.
   */
  private static boolean isParamArrayEmptyFailureDup( RequestCase requestCase, ParamData param, String inputId)
    {
    return
      // True for any invalid empty array value...
      requestCase.getInvalidInput().equals( String.format( "%s.Items.Size=0", inputId))

      // ... except for the value of a form-encoded array parameter
      && !(inputId.equals( toIdentifier( param.getName()))
           && (param.getLocation() == Location.QUERY || param.getLocation() == Location.COOKIE));
    }

  /**
   * Returns if the given request case represents a parameter failure caused by an empty string that
   * duplicates a null value when serialized.
   */
  private static boolean isParamStringEmptyFailureDup( RequestCase requestCase, ParamData param, String inputId)
    {
    return
      // True for any invalid empty string value...
      requestCase.getInvalidInput().equals( String.format( "%s.Value.Length=0", inputId))

      // ... except for the value of a form-encoded string parameter
      && !(inputId.equals( toIdentifier( param.getName()))
           && (param.getLocation() == Location.QUERY || param.getLocation() == Location.COOKIE));
    }

  /**
   * Returns if the given request case represents a parameter failure caused by an undefined value that
   * duplicates a null value when serialized.
   */
  private static boolean isParamUndefinedFailureDup( RequestCase requestCase, ParamData param, String inputId, DataValue.Type inputType)
    {
    return
      requestCase.getInvalidInput().equals( String.format( "%s.Defined=No", inputId))
      && inputId.equals( toIdentifier( param.getName()))
      && param.getStyle().equals( "simple")
      && param.getLocation() == Location.PATH;
    }

  /**
   * If the given request case is a failure case with an invalid parameter value, return the invalid parameter data.
   */
  private static Optional<ParamData> getParamFailureData( RequestCase requestCase)
    {
    return
      Optional.of( requestCase)
      .filter( RequestCase::isFailure)
      .map( rc -> toStream( rc.getParams()))
      .orElse( Stream.empty())
      .filter( param -> !param.isValid())
      .findFirst();
    }

  /**
   * Returns if the given request case represents a failure caused by a body value that
   * duplicates a null value when serialized.
   */
  private static boolean isBodyFailureDup( RequestCase requestCase, String inputId, DataValue.Type inputType)
    {
    return
      getBodyFailureData( requestCase)
      .map( body -> {
        return
          Optional.ofNullable( body.getMediaType())
          .map( MediaRange::of)
          .filter( mediaType -> !("application/json".equals( mediaType.base()) || "json".equals( mediaType.suffix())))
          .map( mediaType -> {
            return
              isBodyObjectEmptyFailureDup( requestCase, body, inputId)
              || isBodyArrayEmptyFailureDup( requestCase, body, inputId)
              || isBodyStringEmptyFailureDup( requestCase, body, inputId);
            })
          .orElse( false);
        })
      .orElse( false);
    }

  /**
   * Returns if the given request case represents a body failure caused by an empty object that
   * duplicates a null value when serialized.
   */
  private static boolean isBodyObjectEmptyFailureDup( RequestCase requestCase, MessageData body, String inputId)
    {
    Optional<ObjectValue> invalidObject =
      Optional.of( body.getValue())
      .filter( value -> requestCase.getInvalidInput().startsWith( String.format( "%s.Value.Property-Count=<", inputId)))
      .map( ObjectValue.class::cast);

    return
      invalidObject
      .filter( object -> object.getValue().isEmpty())
      .map( object -> !isFormProperty( body, inputId))
      .orElse( false);
    }

  /**
   * Returns if the given request case represents a body failure caused by an empty array that
   * duplicates a null value when serialized.
   */
  private static boolean isBodyArrayEmptyFailureDup( RequestCase requestCase, MessageData body, String inputId)
    {
    return
      requestCase.getInvalidInput().equals( String.format( "%s.Items.Size=0", inputId))
      && !isFormProperty( body, inputId);
    }

  /**
   * Returns if the given request case represents a body failure caused by an empty string that
   * duplicates a null value when serialized.
   */
  private static boolean isBodyStringEmptyFailureDup( RequestCase requestCase, MessageData body, String inputId)
    {
    return
      requestCase.getInvalidInput().equals( String.format( "%s.Value.Length=0", inputId))
      && !isFormProperty( body, inputId);
    }

  /**
   * Returns if the given request input is a property of an <CODE>application/x-www-form-urlencoded</CODE> object.
   */
  private static boolean isFormProperty( MessageData body, String inputId)
    {
    // Is this input a property of an object...?
    return
      objectWithProperty( Arrays.stream( inputId.split( "\\.", -1)).collect( toList()))
      .map( objectPath -> {
        return
          // ... which is an application/x-www-form-urlencoded form?
          objectPath.equals( Arrays.asList( "Body", "application-x-www-form-urlencoded"))
          ||
          // ... or is the object containing the property input itself a part of another object...?
          objectWithProperty( objectPath)
          .map( partPath -> {
            return
              objectWithProperty( partPath)
              .map( formPath -> {
                String partName = partPath.get( partPath.size() - 1);
                return
                  // ... which is a multipart/form-data form part...?
                  formPath.equals( Arrays.asList( "Body", "multipart-form-data"))
                  &&
                  // ... that is an application/x-www-form-urlencoded form?
                  body.getEncodings()
                  .entrySet().stream()
                  .filter( encoding -> partName.equals( toIdentifier( encoding.getKey())))
                  .findFirst()
                  .map( encoding -> "application/x-www-form-urlencoded".equals( encoding.getValue().getContentType()))
                  .orElse( false);

                })
              .orElse( false);
            })
          .orElse( false);
        })
      .orElse( false);
    }

  /**
   * If the given input path identifies an object property, returns the path to the object.
   * Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  private static Optional<List<String>> objectWithProperty( List<String> inputPath)
    {
    return
      Optional.ofNullable( inputPath)
      .map( path -> path.subList( 0, path.size() - 1))
      .filter( parent -> parent.size() >= 2 && parent.subList( parent.size() - 2, parent.size()).equals( Arrays.asList( "Value", "Properties"))) 
      .map( parent -> parent.subList( 0, parent.size() - 2));
    }

  /**
   * If the given request case is a failure case with an invalid body value, return the invalid body data.
   */
  private static Optional<MessageData> getBodyFailureData( RequestCase requestCase)
    {
    return
      Optional.of( requestCase)
      .filter( RequestCase::isFailure)
      .filter( rc -> toStream( rc.getParams()).allMatch( MessageData::isValid))
      .map( rc -> rc.getBody());
    }
  }
