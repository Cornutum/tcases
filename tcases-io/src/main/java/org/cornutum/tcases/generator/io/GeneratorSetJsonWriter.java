//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.util.MapBuilder;
import static org.cornutum.tcases.generator.io.GeneratorSetJson.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

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
import javax.json.JsonObjectBuilder;
import javax.json.JsonStructure;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import static javax.json.stream.JsonGenerator.PRETTY_PRINTING;

/**
 * Writes an {@link IGeneratorSet} in the form of a JSON document.
 *
 */
public class GeneratorSetJsonWriter implements Closeable
  {
  /**
   * Creates a new GeneratorSetJsonWriter object that writes to standard output.
   */
  public GeneratorSetJsonWriter()
    {
    this( (Writer) null);
    }
  
  /**
   * Creates a new GeneratorSetJsonWriter object that writes to the given stream.
   */
  public GeneratorSetJsonWriter( OutputStream stream)
    {
    this( writerFor( stream));
    }
  
  /**
   * Creates a new GeneratorSetJsonWriter object that writes to the given stream.
   */
  public GeneratorSetJsonWriter( Writer writer)
    {
    setWriter( writer);
    }

  /**
   * Writes the given system test definition the form of a JSON document.
   */
  public void write( IGeneratorSet generatorSet)
    {
    JsonWriterFactory writerFactory = Json.createWriterFactory( MapBuilder.of( PRETTY_PRINTING, true).build());
    JsonWriter jsonWriter = writerFactory.createWriter( getWriter());

    jsonWriter.write( toJson( generatorSet));
    }

  /**
   * Returns the JSON object that represents the given generator set.
   */
  private JsonStructure toJson( IGeneratorSet generatorSet)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    for( String function : generatorSet.getGeneratorFunctions())
      {
      builder.add( function, toJson( generatorSet.getGenerator( function)));
      }

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given generator.
   */
  private JsonStructure toJson( ITestCaseGenerator generator)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    if( generator.getClass().equals( TupleGenerator.class))
      {
      TupleGenerator tupleGenerator = (TupleGenerator) generator;
      builder.add( TUPLES_KEY, tupleGenerator.getDefaultTupleSize());
      Optional.ofNullable( tupleGenerator.getRandomSeed()).ifPresent( seed -> builder.add( SEED_KEY, seed));
      
      JsonArrayBuilder combinersBuilder = Json.createArrayBuilder();
      tupleGenerator.getCombiners().stream().forEach( combiner -> combinersBuilder.add( toJson( combiner)));
      JsonArray combiners = combinersBuilder.build();
      if( !combiners.isEmpty())
        {
        builder.add( COMBINERS_KEY, combiners);
        }
      }

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given combiner.
   */
  private JsonStructure toJson( TupleCombiner combiner)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( TUPLES_KEY, combiner.getTupleSize());

    JsonArrayBuilder includeBuilder = Json.createArrayBuilder();
    Arrays.stream( combiner.getIncluded()).forEach( var -> includeBuilder.add( var));
    JsonArray include = includeBuilder.build();
    if( !include.isEmpty())
      {
      builder.add( INCLUDE_KEY, include);
      }

    JsonArrayBuilder excludeBuilder = Json.createArrayBuilder();
    Arrays.stream( combiner.getExcluded()).forEach( var -> excludeBuilder.add( var));
    JsonArray exclude = excludeBuilder.build();
    if( !exclude.isEmpty())
      {
      builder.add( EXCLUDE_KEY, exclude);
      }

    JsonArrayBuilder onceBuilder = Json.createArrayBuilder();
    toStream( combiner.getOnceTuples()).forEach( tuple -> onceBuilder.add( toJson( tuple)));
    JsonArray once = onceBuilder.build();
    if( !once.isEmpty())
      {
      builder.add( ONCE_KEY, once);
      }

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given once tuple.
   */
  private JsonStructure toJson( TupleRef tuple)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    toStream( tuple.getVarBindings()).forEach( binding -> builder.add( binding.getVar(), String.valueOf( binding.getValue())));
    
    return builder.build();
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

  private Writer writer_;  
  }
