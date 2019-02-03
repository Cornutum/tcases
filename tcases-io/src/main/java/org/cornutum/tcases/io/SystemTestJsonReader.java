//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.DefUtils;
import static org.cornutum.tcases.io.SystemTestJson.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import java.util.Optional;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;

/**
 * An {@link ISystemTestSource} that reads from an JSON document.
 *
 */
public class SystemTestJsonReader implements ISystemTestSource, Closeable
  {  
  /**
   * Creates a new SystemTestJsonReader object.
   */
  public SystemTestJsonReader()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemTestJsonReader object.
   */
  public SystemTestJsonReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link SystemTestDef} instance.
   */
  public SystemTestDef getSystemTestDef()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/system-test-schema.json"));
    ProblemHandler handler = ProblemHandler.throwing();
    try( JsonReader reader = service.createReader( stream_, schema, handler))
      {
      JsonObject json;
      try
        {
        json = reader.readObject();
        }
      catch( Exception e)
        {
        throw new SystemTestException( "Invalid system test definition", e);
        }

      return asSystemTestDef( json);
      }
    }

  /**
   * Changes the input stream for this reader.
   */
  public void setInputStream( InputStream stream)
    {
    stream_ =
      stream==null
      ? System.in
      : stream;
    }

  /**
   * Returns the SystemTestDef represented by the given JSON object.
   */
  private SystemTestDef asSystemTestDef( JsonObject json)
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
        .forEach( function -> systemTestDef.addFunctionTestDef( asFunctionTestDef( function, json.getJsonArray( function))));

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
  private FunctionTestDef asFunctionTestDef( String functionName, JsonArray json)
    {
    FunctionTestDef functionTestDef;

    try
      {
      functionTestDef = new FunctionTestDef( validIdentifier( functionName));

      // Get function test cases.
      json.getValuesAs( JsonObject.class)
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
  private TestCase asTestCase( JsonObject json)
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
  private Stream<VarBinding> getVarBindings( String varType, JsonObject json)
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
  private VarBinding asVarBinding( String varName, String varType, JsonObject json)
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
  private String validIdentifier( String string)
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
  private String validPath( String string)
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
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return stream_;
    }

  public void close()
    {
    IOUtils.closeQuietly( getInputStream());
    }

  private InputStream stream_;
  }
