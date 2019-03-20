//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.DefUtils;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonStructure;

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
        .forEach( function -> systemTestDef.addFunctionTestDef( asFunctionTestDef( function, json.getJsonObject( function))));

      return systemTestDef;
      }
    catch( SystemTestException e)
      {
      throw new SystemTestException( String.format( "Error defining system=%s", systemName), e);
      }
    }

  /**
   * Returns the FunctionTestDef represented by the given JSON array.
   */
  private static FunctionTestDef asFunctionTestDef( String functionName, JsonObject json)
    {
    FunctionTestDef functionTestDef;

    try
      {
      functionTestDef = new FunctionTestDef( validIdentifier( functionName));

      // Get annotations for this function.
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> functionTestDef.setAnnotation( key, has.getString( key))));

      // Get function test cases.
      json.getJsonArray( TEST_CASES_KEY)
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

    try
      {
      // Get annotations for this test case.
      Optional.ofNullable( json.getJsonObject( HAS_KEY))
        .ifPresent( has -> has.keySet().stream().forEach( key -> testCase.setAnnotation( key, has.getString( key))));

      // Get test case bindings.
      json.keySet().stream()
        .filter( key -> !(key.equals( ID_KEY) || key.equals( HAS_KEY)))
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
          .value( json.getString( VALUE_KEY))
          .valid( !json.getBoolean( FAILURE_KEY, false));
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
      if( !binding.isValueValid())
        {
        builder.add( FAILURE_KEY, true);
        }
      builder.add( VALUE_KEY, String.valueOf( binding.getValue()));
      }

    return builder.build();
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
  private static final String NA_KEY = "NA";
  private static final String SYSTEM_KEY = "system";
  private static final String TEST_CASES_KEY = "testCases";
  private static final String VALUE_KEY = "value";
}
