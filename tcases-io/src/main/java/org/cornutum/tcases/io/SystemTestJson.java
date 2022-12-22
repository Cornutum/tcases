//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.DefUtils;
import org.cornutum.tcases.util.ObjectUtils;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import static javax.json.JsonValue.ValueType.*;

/**
 * Converts between a {@link SystemTestDef} and its corresponding {@link JsonObject}.
 */
public final class SystemTestJson
  {
  private SystemTestJson()
    {
    // Static methods only
    }

  /**
   * Returns the SystemTestDef represented by the given JSON object.
   */
  public static SystemTestDef asSystemTestDef( JsonObject json)
    {
    String systemName = json.getString( SYSTEM_KEY);
    try
      {
      SystemTestDef systemTestDef = new SystemTestDef( validIdentifier( systemName));
      
      // Get annotations for this system.
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> systemTestDef.setAnnotation( key, has.getString( key))));

      // Get function test definitions
      json.keySet().stream()
        .filter( key -> !(key.equals( SYSTEM_KEY) || key.equals( HAS_KEY)))
        .forEach( function -> systemTestDef.addFunctionTestDef( asFunctionTestDef( function, json.getValue( String.format( "/%s", function)))));

      return systemTestDef;
      }
    catch( SystemTestException e)
      {
      throw new SystemTestException( String.format( "Error defining system=%s", systemName), e);
      }
    }

  /**
   * Returns the FunctionTestDef represented by the given JSON value.
   */
  private static FunctionTestDef asFunctionTestDef( String functionName, JsonValue json)
    {
    FunctionTestDef functionTestDef;

    try
      {
      functionTestDef = new FunctionTestDef( validIdentifier( functionName));

      JsonArray testCases;
      if( json.getValueType() == ARRAY)
        {
        // For compatibility, accept documents conforming to schema version <= 3.0.1
        testCases = json.asJsonArray();
        }
      else
        {
        JsonObject functionObject = json.asJsonObject();
        testCases = functionObject.getJsonArray( TEST_CASES_KEY);
        
        // Get annotations for this function.
        Optional.ofNullable( functionObject.getJsonObject( HAS_KEY))
          .ifPresent( has -> has.keySet().stream().forEach( key -> functionTestDef.setAnnotation( key, has.getString( key))));
        }

      // Get function test cases.
      testCases
        .getValuesAs( JsonObject.class)
        .stream()
        .forEach( testCase -> functionTestDef.addTestCase( asTestCase( testCase)));
      }
    catch( SystemTestException e)
      {
      throw new SystemTestException( String.format( "Error defining function=%s", functionName), e);
      }
    
    return functionTestDef;
    }

  /**
   * Returns the test case represented by the given JSON object.
   */
  private static TestCase asTestCase( JsonObject json)
    {
    TestCase testCase = new TestCase( json.getInt( ID_KEY));
    testCase.setName( json.getString( NAME_KEY, null));

    try
      {
      // Get annotations for this test case.
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> testCase.setAnnotation( key, has.getString( key))));

      // Get test case bindings.
      json.keySet().stream()
        .filter( key -> !(key.equals( ID_KEY) || key.equals( HAS_KEY) || key.equals( NAME_KEY)))
        .forEach( varType -> getVarBindings( varType, json.getJsonObject( varType)).forEach( binding -> testCase.addVarBinding( binding)));

      long failureCount =
        toStream( testCase.getVarBindings())
        .filter( binding -> !binding.isValueValid())
        .count();
      if( failureCount > 1)
        {
        throw new SystemTestException( "Can't have more than one variable bound to a value with failure=true");
        }
      
      return testCase;
      }
    catch( SystemTestException e)
      {
      throw new SystemTestException( String.format( "Error defining test case=%s", testCase.getId()), e);
      }
    }

  /**
   * Returns the variable bindings represented by the given JSON object.
   */
  private static Stream<VarBinding> getVarBindings( String varType, JsonObject json)
    {
    try
      {
      // Return bindings for this variable type.
      return
        json.keySet().stream()
        .map( varName -> asVarBinding( varName, varType, json.getJsonObject( varName)));
      }
    catch( SystemInputException e)
      {
      throw new SystemInputException( String.format( "Error defining variable bindings of type=%s", varType), e);
      }
    }

  /**
   * Returns the variable binding represented by the given JSON object.
   */
  private static VarBinding asVarBinding( String varName, String varType, JsonObject json)
    {
    try
      {
      VarBindingBuilder builder = VarBindingBuilder.with( validPath(varName)).type( varType);

      if( json.getBoolean( NA_KEY, false))
        {
        builder.notApplicable();
        }
      else
        {
        builder
          .value( asValueObject( json.get( VALUE_KEY)))
          .valid( !json.getBoolean( FAILURE_KEY, false))
          .source( Optional.ofNullable( json.get( SOURCE_KEY)).map( s -> asValueObject( s)).orElse( null));
        }
      
      // Get annotations for this binding
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> builder.has( key, has.getString( key))));

      return builder.build();
      }
    catch( SystemTestException e)
      {
      throw new SystemTestException( String.format( "Error defining binding for variable=%s", varName), e);
      }
    }

  /**
   * Returns the variable value represented by the given JSON value.
   */
  private static Object asValueObject( JsonValue json)
    {
    if( json == null || json.getValueType() == OBJECT)
      {
      throw new SystemTestException( String.format( "Invalid value=%s", json));
      }

    Object object;
    switch( json.getValueType())
      {
      case STRING:
        {
        object = ((JsonString) json).getString();
        break;
        }
      case ARRAY:
        {
        object =
          ((JsonArray) json).stream()
          .map( element -> asValueObject( element))
          .collect( toList());
        break;
        }
      default:
        {
        object = ObjectUtils.toObject( String.valueOf( json));
        break;
        }
      }

    return object;
    }

  /**
   * Reports a SystemTestException if the given string is not a valid identifier. Otherwise, returns this string.
   */
  private static String validIdentifier( String string)
    {
    try
      {
      DefUtils.assertIdentifier( string);
      }
    catch( Exception e)
      {
      throw new SystemTestException( e.getMessage());
      }

    return string;
    }

  /**
   * Reports a SystemTestException if the given string is not a valid variable path. Otherwise, returns this string.
   */
  private static String validPath( String string)
    {
    try
      {
      DefUtils.assertPath( string);
      }
    catch( Exception e)
      {
      throw new SystemTestException( e.getMessage());
      }

    return string;
    }

  /**
   * Returns the JSON object that represents the given system test definition.
   */
  public static JsonObject toJson( SystemTestDef systemTest)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    builder.add( SYSTEM_KEY, systemTest.getName());

    addAnnotations( builder, systemTest);
    
    toStream( systemTest.getFunctionTestDefs()).forEach( function -> builder.add( function.getName(), toJson( function)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given function test definition.
   */
  private static JsonStructure toJson( FunctionTestDef functionTest)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, functionTest);
    
    JsonArrayBuilder testCases = Json.createArrayBuilder();
    toStream( functionTest.getTestCases()).forEach( testCase -> testCases.add( toJson( testCase)));
    builder.add( TEST_CASES_KEY, testCases);

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given test case.
   */
  private static JsonStructure toJson( TestCase testCase)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( ID_KEY, testCase.getId());
    Optional.ofNullable( testCase.getName()).ifPresent( name -> builder.add( NAME_KEY, name));
    addAnnotations( builder, testCase);

    Arrays.stream( testCase.getVarTypes()).forEach( varType -> builder.add( varType, toJson( testCase, varType)));
    
    return builder.build();
    }

  /**
   * Returns the JSON object that represents the bindings for variables of the given type.
   */
  private static JsonStructure toJson( TestCase testCase, String varType)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    toStream( testCase.getVarBindings( varType))
      .sorted()
      .forEach( binding -> builder.add( binding.getVar(), toJson( binding)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given variable binding.
   */
  private static JsonStructure toJson( VarBinding binding)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, binding);
    
    if( binding.isValueNA())
      {
      builder.add( NA_KEY, true);
      }
    else
      {
      Object value = binding.getValue();
      Object source = binding.getSource();

      if( !binding.isValueValid())
        {
        builder.add( FAILURE_KEY, true);
        }
      builder.add( VALUE_KEY, toJsonValue( value));
      if( source != value)
        {
        builder.add( SOURCE_KEY, toJsonValue( source));
        }
      }

    return builder.build();
    }

  /**
   * Returns the JSON value that represents the given variable value.
   */
  private static JsonValue toJsonValue( Object value)
    {
    JsonValue json;

    Class<?> type = Optional.ofNullable( value).map( Object::getClass).orElse( null);
    if( type == null)
      {
      json = JsonValue.NULL;
      }
    else if( type.equals( Boolean.class))
      {
      json = ((Boolean) value)? JsonValue.TRUE : JsonValue.FALSE;
      }
    else if( type.equals( String.class))
      {
      json = Json.createValue( (String) value);
      }
    else if( type.equals( Integer.class))
      {
      json = Json.createValue( (Integer) value);
      }
    else if( type.equals( Long.class))
      {
      json = Json.createValue( (Long) value);
      }
    else if( type.equals( BigDecimal.class))
      {
      json = Json.createValue( (BigDecimal) value);
      }
    else if( Number.class.isAssignableFrom( type))
      {
      json = Json.createValue( new BigDecimal( ((Number) value).doubleValue()));
      }
    else if( Collection.class.isAssignableFrom( type))
      {
      JsonArrayBuilder array = Json.createArrayBuilder();
      ((Collection<?>) value).stream().forEach( element -> array.add( toJsonValue( element)));
      json = array.build();
      }
    else
      {
      json = Json.createValue( String.valueOf( value));
      }

    return json;
    }

  /**
   * Add any annotatations from the given Annotated object to the given JsonObjectBuilder.
   */
  private static JsonObjectBuilder addAnnotations( JsonObjectBuilder builder, IAnnotated annotated)
    {
    JsonObjectBuilder annotations = Json.createObjectBuilder();
    toStream( annotated.getAnnotations()).forEach( name -> annotations.add( name, annotated.getAnnotation( name)));
    JsonObject json = annotations.build();

    if( !json.isEmpty())
      {
      builder.add( HAS_KEY, json);
      }

    return builder;
    }

  private static final String FAILURE_KEY = "failure";
  private static final String HAS_KEY = "has";
  private static final String ID_KEY = "id";
  private static final String NAME_KEY = "name";
  private static final String NA_KEY = "NA";
  private static final String SOURCE_KEY = "source";
  private static final String SYSTEM_KEY = "system";
  private static final String TEST_CASES_KEY = "testCases";
  private static final String VALUE_KEY = "value";
}
