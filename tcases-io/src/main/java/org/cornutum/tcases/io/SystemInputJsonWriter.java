//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;
import org.cornutum.tcases.util.MapBuilder;
import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.io.SystemInputJson.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.io.IOUtils;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonStructure;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import static javax.json.stream.JsonGenerator.PRETTY_PRINTING;

/**
 * Writes a {@link SystemInputDef} in the form of a JSON document.
 *
 */
public class SystemInputJsonWriter implements Closeable
  {
  /**
   * Creates a new SystemInputJsonWriter object that writes to standard output.
   */
  public SystemInputJsonWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new SystemInputJsonWriter object that writes to the given stream.
   */
  public SystemInputJsonWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new SystemInputJsonWriter object that writes to the given stream.
   */
  public SystemInputJsonWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given system test definition the form of a JSON document.
   */
  public void write( SystemInputDef systemInput)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( toJson( systemInput));
    }

  /**
   * Returns the JSON object that represents the given system input definition.
   */
  private JsonStructure toJson( SystemInputDef systemInput)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    builder.add( SYSTEM_KEY, systemInput.getName());

    addAnnotations( builder, systemInput);
    
    toStream( systemInput.getFunctionInputDefs()).forEach( function -> builder.add( function.getName(), toJson( function)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given function input definition.
   */
  private JsonStructure toJson( FunctionInputDef functionInput)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, functionInput);

    Arrays.stream( functionInput.getVarTypes()).forEach( varType -> builder.add( varType, toJson( functionInput, varType)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the function variables of the given type.
   */
  private JsonStructure toJson( FunctionInputDef functionInput, String varType)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();
    toStream( functionInput.getVarDefs())
      .filter( varDef -> varDef.getType().equals( varType))
      .sorted()
      .forEach( varDef -> builder.add( varDef.getName(), toJson( varDef)));

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given variable definition.
   */
  private JsonStructure toJson( IVarDef varDef)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    addAnnotations( builder, varDef);

    ConditionJson.toJson( varDef).ifPresent( json -> builder.add( WHEN_KEY, json));

    if( varDef.getValues() != null)
      {
      JsonObjectBuilder valuesBuilder = Json.createObjectBuilder();
      toStream( varDef.getValues()).forEach( value -> valuesBuilder.add( String.valueOf( value.getName()), toJson( value)));
      builder.add( VALUES_KEY, valuesBuilder.build());
      }
    else if( varDef.getMembers() != null)
      {
      JsonObjectBuilder membersBuilder = Json.createObjectBuilder();
      toStream( varDef.getMembers()).forEach( member -> membersBuilder.add( member.getName(), toJson( member)));
      builder.add( MEMBERS_KEY, membersBuilder.build());
      }
      
    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given value definition.
   */
  private JsonStructure toJson( VarValueDef value)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    if( value.getType().equals( FAILURE))
      {
      builder.add( FAILURE_KEY, true);
      }
    else if( value.getType().equals( ONCE))
      {
      builder.add( ONCE_KEY, true);
      }

    addAnnotations( builder, value);

    ConditionJson.toJson( value).ifPresent( json -> builder.add( WHEN_KEY, json));
      
    addProperties( builder, value);

    return builder.build();
    }

  /**
   * Add any annotatations from the given Annotated object to the given JsonObjectBuilder.
   */
  private JsonObjectBuilder addAnnotations( JsonObjectBuilder builder, IAnnotated annotated)
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

  /**
   * Add any properties from the given value to the given JsonObjectBuilder.
   */
  private JsonObjectBuilder addProperties( JsonObjectBuilder builder, VarValueDef value)
    {
    JsonArrayBuilder properties = Json.createArrayBuilder();
    toStream( value.getProperties().getProperties()).forEach( property -> properties.add( property));
    JsonArray json = properties.build();

    if( !json.isEmpty())
      {
      builder.add( PROPERTIES_KEY, json);
      }

    return builder;
    }

  /**
   * Flushes the writer.
   */
  public void flush()
    {
    try
      {
      getWriter().flush();
      }
    catch( IOException ignore)
      {
      }
    }

  /**
   * Closes the writer.
   */
  public void close()
    {
    IOUtils.closeQuietly( getWriter());
    }

  /**
   * Changes the output stream for this writer.
   */
  protected void setWriter( Writer writer)
    {
    writer_ =
      writer == null
      ? writerFor( System.out)
      : writer;
    }

  /**
   * Returns the output stream for this writer.
   */
  protected Writer getWriter()
    {
    return writer_;
    }

  /**
   * Returns a Writer for the given output stream;
   */
  private static Writer writerFor( OutputStream stream)
    {
    try
      {
      return
        stream == null
        ? null
        : new OutputStreamWriter( stream, "UTF-8");
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't create writer", e);
      }
    }

  private static class ConditionJson implements IConditionVisitor
    {
    public static Optional<JsonObject> toJson( IConditional conditional)
      {
      return
        Optional.ofNullable( conditional.getCondition())
        .map( condition -> ConditionJson.toJson( condition));
      }

    private static JsonObject toJson( ICondition condition)
      {
      return new ConditionJson( condition).toJson();
      }
    
    private ConditionJson( ICondition condition)
      {
      condition_ = condition;
      }
    
    private JsonObject toJson()
      {
      condition_.accept( this);
      return json_;
      }
    
    public void visit( AllOf condition)
      {
      JsonArrayBuilder conditions = Json.createArrayBuilder();
      toStream( condition.getConditions()).forEach( c -> conditions.add( toJson( c)));

      json_ =
        Json.createObjectBuilder()
        .add( ALL_OF_KEY, conditions)
        .build();
      }
  
    public void visit( AnyOf condition)
      {
      JsonArrayBuilder conditions = Json.createArrayBuilder();
      toStream( condition.getConditions()).forEach( c -> conditions.add( toJson( c)));

      json_ =
        Json.createObjectBuilder()
        .add( ANY_OF_KEY, conditions)
        .build();
      }
  
    public void visit( ContainsAll condition)
      {
      JsonArrayBuilder properties = Json.createArrayBuilder();
      toStream( condition.getProperties()).forEach( property -> properties.add( property));

      json_ =
        Json.createObjectBuilder()
        .add( HAS_ALL_KEY, properties)
        .build();
      }
  
    public void visit( ContainsAny condition)
      {
      JsonArrayBuilder properties = Json.createArrayBuilder();
      toStream( condition.getProperties()).forEach( property -> properties.add( property));

      json_ =
        Json.createObjectBuilder()
        .add( HAS_ANY_KEY, properties)
        .build();
      }
  
    public void visit( IConjunct condition)
      {
      throw new UnsupportedOperationException( "Unexpected IConjunct in SystemInputDef");
      }
  
    public void visit( Not condition)
      {
      ICondition[] conditions = IteratorUtils.toArray( condition.getConditions(), ICondition.class);

      json_ =
        Json.createObjectBuilder()
        .add(
          NOT_KEY,
          conditions.length == 1
          ? toJson( conditions[0])
          : toJson( new AnyOf( conditions)))
        .build();
      }

    private ICondition condition_;
    private JsonObject json_;
    }

  private Writer writer_;  
  }
