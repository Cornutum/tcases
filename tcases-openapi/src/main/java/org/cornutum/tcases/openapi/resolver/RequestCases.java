/////////////////////////////////////////////////////////////////////////////
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
import static org.cornutum.tcases.util.CollectionUtils.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

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
      .map( body -> !body.isValid() && isJson( body.getMediaType()))
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
    return
      requestCases.stream()
      .filter( RequestCases::isNullFailure)
      .map( RequestCases::nullFailureId)
      .flatMap( id -> requestCases.stream().filter( rc -> isParamFailureDup( rc, id) || isBodyFailureDup( rc, id)))
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
   * Returns if the given request case represents a failure caused by a parameter value that
   * duplicates a null value when serialized.
   */
  private static boolean isParamFailureDup( RequestCase requestCase, String inputId)
    {
    return
      getParamFailureData( requestCase)
      .map( param -> {
        return
          isParamObjectEmptyFailureDup( requestCase, param, inputId)
          || isParamArrayEmptyFailureDup( requestCase, param, inputId)
          || isParamStringEmptyFailureDup( requestCase, param, inputId)
          || isParamUndefinedFailureDup( requestCase, param, inputId);
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
      requestCase.getInvalidInput().startsWith( String.format( "%s.Value.Property-Count=<", inputId))
      && ((ObjectValue) param.getValue()).getValue().isEmpty()
      && isSimpleEncoded( param, inputId);
    }

  /**
   * Returns if the given request case represents a parameter failure caused by an empty array that
   * duplicates a null value when serialized.
   */
  private static boolean isParamArrayEmptyFailureDup( RequestCase requestCase, ParamData param, String inputId)
    {
    return
      requestCase.getInvalidInput().equals( String.format( "%s.Items.Size=0", inputId))
      && isSimpleEncoded( param, inputId);
    }

  /**
   * Returns if the given request case represents a parameter failure caused by an empty string that
   * duplicates a null value when serialized.
   */
  private static boolean isParamStringEmptyFailureDup( RequestCase requestCase, ParamData param, String inputId)
    {
    return
      requestCase.getInvalidInput().equals( String.format( "%s.Value.Length=0", inputId))
      && isSimpleEncoded( param, inputId);
    }

  /**
   * Returns if the given request case represents a parameter failure caused by an undefined value that
   * duplicates a null value when serialized.
   */
  private static boolean isParamUndefinedFailureDup( RequestCase requestCase, ParamData param, String inputId)
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
  private static boolean isBodyFailureDup( RequestCase requestCase, String inputId)
    {
    return
      getBodyFailureData( requestCase)
      .map( body -> {
        return
          !isJson( body.getMediaType())
          &&
          (isBodyObjectEmptyFailureDup( requestCase, body, inputId)
           || isBodyArrayEmptyFailureDup( requestCase, body, inputId)
           || isBodyStringEmptyFailureDup( requestCase, body, inputId));
        })
      .orElse( false);
    }

  /**
   * Returns if the given request case represents a body failure caused by an empty object that
   * duplicates a null value when serialized.
   */
  private static boolean isBodyObjectEmptyFailureDup( RequestCase requestCase, MessageData body, String inputId)
    {
    return
      requestCase.getInvalidInput().startsWith( String.format( "%s.Value.Property-Count=<", inputId))
      && getInputObject( body, inputId).getValue().isEmpty()

      && (isSimpleMediaType( body)
          ||
          isRoot( inputId)
          ||
          isSimpleIfUrlEncodedPropertyExploded( body, inputId)
          .orElseGet(
            () ->
            isSimpleIfMultipartPart( body, inputId)
            .orElseGet(
              () ->
              isSimpleIfMultipartPartPropertyExploded( body, inputId)
              .orElseGet(
                () ->
                isSimpleIfDeepContent( body, inputId)
                .orElse( false)))));
    }

  /**
   * Returns if the given request case represents a body failure caused by an empty array that
   * duplicates a null value when serialized.
   */
  private static boolean isBodyArrayEmptyFailureDup( RequestCase requestCase, MessageData body, String inputId)
    {
    return
      requestCase.getInvalidInput().equals( String.format( "%s.Items.Size=0", inputId))

      && (isSimpleMediaType( body)
          ||
          isRoot( inputId)
          ||
          isSimpleIfUrlEncodedProperty( body, inputId)
          .orElseGet(
            () ->
            isSimpleIfMultipartPart( body, inputId)
            .orElseGet(
            () ->
            isSimpleIfDeepContent( body, inputId)
            .orElse( false))));
    }

  /**
   * Returns if the given request case represents a body failure caused by an empty string that
   * duplicates a null value when serialized.
   */
  private static boolean isBodyStringEmptyFailureDup( RequestCase requestCase, MessageData body, String inputId)
    {
    return
      requestCase.getInvalidInput().equals( String.format( "%s.Value.Length=0", inputId))

      && (isSimpleMediaType( body)
          ||
          isRoot( inputId)
          ||
          isSimpleIfUrlEncodedProperty( body, inputId)
          .orElseGet(
            () ->
            isSimpleIfMultipartPart( body, inputId)
            .orElseGet(
            () ->
            isSimpleIfDeepContent( body, inputId)
            .orElse( false))));
    }

  /**
   * TBD
   */
  private static ObjectValue getInputObject( MessageData body, String inputId)
    {
    ObjectValue bodyObject = (ObjectValue) body.getValue();

    return
      Optional.of( URLENCODED_PROPERTY_ID.matcher( inputId))
      .filter( matcher -> matcher.find())
      .flatMap( matcher -> propertyValueFor( bodyObject, matcher.group(1)))
      .map( value -> (ObjectValue) value)

      .orElseGet(
        () ->
        Optional.of( MULTIPART_PART_ID.matcher( inputId))
        .filter( matcher -> matcher.find())
        .flatMap( matcher -> propertyValueFor( bodyObject, matcher.group(1)))
        .map( value -> (ObjectValue) value)

        .orElseGet(
          () ->
          Optional.of( MULTIPART_PART_PROPERTY_ID.matcher( inputId))
          .filter( matcher -> matcher.find())
          .flatMap( matcher -> {
            return
              propertyValueFor( bodyObject, matcher.group(1))
              .map( value -> (ObjectValue) value)
              .flatMap( part -> propertyValueFor( part, matcher.group(2)))
              .map( value -> (ObjectValue) value);
            })

          .orElse( bodyObject)));
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

  /**
   * Returns if the given request parameter input uses a "simple" serialization style. 
   */
  private static boolean isSimpleEncoded( ParamData param, String inputId)
    {
    // True except for the value of an unexploded form-encoded object parameter
    return
      !(inputId.equals( toIdentifier( param.getName()))
        && (param.getLocation() == Location.QUERY || param.getLocation() == Location.COOKIE)
        && !param.isExploded());
    }

  /**
   * Returns if the body has a simple media type.
   */
  private static boolean isSimpleMediaType( MessageData body)
    {
    return
      Optional.ofNullable( body.getMediaType())
      .map( mediaType -> !("application/x-www-form-urlencoded".equals( mediaType) || "multipart/form-data".equals( mediaType)))
      .orElse( true);
    }

  /**
   * Returns if the given input id identifies the root value of body content.
   */
  private static boolean isRoot( String inputId)
    {
    return ROOT_ID.matcher( inputId).find();
    }

  /**
   * If the given input id identifies an exploded property of an <CODE>application/x-www-form-urlencoded</CODE> object,
   * returns if it is simple-encoded. Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  private static Optional<Boolean> isSimpleIfUrlEncodedPropertyExploded( MessageData body, String inputId)
    {
    return
      Optional.of( URLENCODED_PROPERTY_ID.matcher( inputId))
      .filter( matcher -> matcher.find())
      .map( matcher -> matcher.group(1))
      .map( property -> encodingFor( body, property).map( EncodingData::isExploded).orElse( true));
    }

  /**
   * If the given input id identifies a property of an <CODE>application/x-www-form-urlencoded</CODE> object,
   * returns if it is simple-encoded. Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  private static Optional<Boolean> isSimpleIfUrlEncodedProperty( MessageData body, String inputId)
    {
    return
      Optional.of( URLENCODED_PROPERTY_ID.matcher( inputId))
      .filter( matcher -> matcher.find())
      .map( matcher -> false);
    }

  /**
   * If the given input id identifies a part of a <CODE>multipart/form-data</CODE> object,
   * returns if it is simple-encoded. Otherwise, returns <CODE>Optional.empty()</CODE>. 
   */
  private static Optional<Boolean> isSimpleIfMultipartPart( MessageData body, String inputId)
    {
    return
      Optional.of( MULTIPART_PART_ID.matcher( inputId))
      .filter( matcher -> matcher.find())
      .map( matcher -> matcher.group(1))
      .map( property -> encodingFor( body, property).map( EncodingData::getContentType).orElse( "text/plain"))
      .map( contentType -> !isJson( contentType));
    }

  /**
   * If the given input id identifies an exploded property of an <CODE>application/x-www-form-urlencoded</CODE> part of a
   * <CODE>multipart/form-data</CODE> object, returns if it is simple-encoded. Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  private static Optional<Boolean> isSimpleIfMultipartPartPropertyExploded( MessageData body, String inputId)
    {
    return
      Optional.of( MULTIPART_PART_PROPERTY_ID.matcher( inputId))
      .filter( matcher -> matcher.find())
      .map( matcher -> matcher.group(1))
      .map( property -> encodingFor( body, property).map( EncodingData::getContentType).orElse( "text/plain"))
      .map( contentType -> "application/x-www-form-urlencoded".equals( contentType));
    }

  /**
   * If the given input id identifies a deeply-nested value, returns if it is simple-encoded. Otherwise, returns
   * <CODE>Optional.empty()</CODE>.
   */
  private static Optional<Boolean> isSimpleIfDeepContent( MessageData body, String inputId)
    {
    return
      Optional.of( inputId)
      .filter( id -> DEEP_CONTENT_ID.matcher( id).find())
      .map( id -> {
        return
          Optional.of( MULTIPART_CONTENT_ID.matcher( id))
          .filter( matcher -> matcher.find())
          .map( matcher -> matcher.group(1))
          .map( property -> encodingFor( body, property).map( EncodingData::getContentType).orElse( "text/plain"))
          .map( contentType -> !isJson( contentType))
          .orElse( true);
        });
    }

  /**
   * Returns the encoding for the given object property identified by the given property id.
   */
  private static Optional<EncodingData> encodingFor( MessageData body, String propertyId)
    {
    return
      body.getEncodings()
      .entrySet().stream()
      .filter( encoding -> propertyId.equals( toIdentifier( encoding.getKey())))
      .findFirst()
      .map( encoding -> encoding.getValue());
    }

  /**
   * Returns the value of the object property identified by the given property id.
   */
  private static Optional<DataValue<?>> propertyValueFor( ObjectValue object, String propertyId)
    {
    return
      object.getValue()
      .entrySet().stream()
      .filter( e -> propertyId.equals( toIdentifier( e.getKey())))
      .findFirst()
      .map( e -> e.getValue());
    }

  /**
   * Returns if the given media type denotes JSON-encoded content.
   */
  private static boolean isJson( String mediaType)
    {
    return
      Optional.ofNullable( mediaType)
      .map( MediaRange::of)
      .map( media -> "application/json".equals( media.base()) || "json".equals( media.suffix()))
      .orElse( false);
    }

  /**
   * Returns a pattern that matches the begining of an body input id.
   */
  private static Pattern bodyIdPrefix( String... segments)
    {
    return bodyIdPattern( false, segments);
    }

  /**
   * Returns a pattern that matches an entire body input id. 
   */
  private static Pattern bodyIdPattern( String... segments)
    {
    return bodyIdPattern( true, segments);
    }

  /**
   * Returns a pattern that matches a body input id. If <CODE>all</CODE> is true, returns a pattern that matches the entire
   * id. Otherwise, returns a pattern that matches the beginning of the id.
   */
  private static Pattern bodyIdPattern( boolean all, String... segments)
    {
    return Pattern.compile( Arrays.stream( segments).collect( joining( "\\.", "^Body\\.", all? "$" : "")));
    }

  private static final String ANY_NAME = "([^\\.]+)";
  private static final String ANY_NAMES = String.format( "%s(\\.%s)+", ANY_NAME, ANY_NAME);

  private static final Pattern DEEP_CONTENT_ID = bodyIdPrefix( ANY_NAME, "Value", ANY_NAMES, "Value");
  private static final Pattern MULTIPART_CONTENT_ID = bodyIdPrefix( "multipart-form-data", "Value", "Properties", ANY_NAME);
  private static final Pattern MULTIPART_PART_ID = bodyIdPattern( "multipart-form-data", "Value", "Properties", ANY_NAME);
  private static final Pattern MULTIPART_PART_PROPERTY_ID = bodyIdPattern( "multipart-form-data", "Value", "Properties", ANY_NAME, "Value", "Properties", ANY_NAME);
  private static final Pattern ROOT_ID = bodyIdPattern( ANY_NAME);
  private static final Pattern URLENCODED_PROPERTY_ID = bodyIdPattern( "application-x-www-form-urlencoded", "Value", "Properties", ANY_NAME);

  }
