//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.DefUtils;
import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.io.ObjectUtils;

import static org.cornutum.tcases.generator.io.GeneratorSetJson.*;
import org.apache.commons.io.IOUtils;
import org.leadpony.justify.api.JsonSchema;
import org.leadpony.justify.api.JsonValidationService;
import org.leadpony.justify.api.ProblemHandler;

import java.io.Closeable;
import java.io.InputStream;
import java.util.Optional;
import java.util.stream.Stream;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonString;

/**
 * An {@link IGeneratorSetSource} that reads from an JSON document.
 *
 */
public class GeneratorSetJsonReader implements IGeneratorSetSource, Closeable
  {  
  /**
   * Creates a new GeneratorSetJsonReader object.
   */
  public GeneratorSetJsonReader()
    {
    this( null);
    }
  
  /**
   * Creates a new GeneratorSetJsonReader object.
   */
  public GeneratorSetJsonReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link IGeneratorSet} instance.
   */
  public IGeneratorSet getGeneratorSet()
    {
    JsonValidationService service = JsonValidationService.newInstance();
    JsonSchema schema = service.readSchema( getClass().getResourceAsStream( "/schema/generators-schema.json"));
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
        throw new GeneratorSetException( "Invalid generator set definition", e);
        }

      return asGeneratorSet( json);
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
   * Returns the IGeneratorSet represented by the given JSON object.
   */
  private IGeneratorSet asGeneratorSet( JsonObject json)
    {
    GeneratorSet generatorSet = new GeneratorSet();

    // Get generator definitions
    json.keySet().stream()
      .forEach( function -> {
        try
          {
          generatorSet.addGenerator( validFunction( function), asGenerator( json.getJsonObject( function)));
          }
        catch( GeneratorSetException e)
          {
          throw new GeneratorSetException( String.format( "Error defining generator for function=%s", function), e);
          }
        });

    return generatorSet;
    }

  /**
   * Returns the generator represented by the given JSON object.
   */
  private TupleGenerator asGenerator( JsonObject json)
    {
    TupleGenerator tupleGenerator = new TupleGenerator();

    Optional.ofNullable( json.getJsonNumber( TUPLES_KEY))
      .ifPresent( n -> tupleGenerator.setDefaultTupleSize( n.intValue()));

    Optional.ofNullable( json.getJsonNumber( SEED_KEY))
      .ifPresent( n -> tupleGenerator.setRandomSeed( n.longValue()));

    // Get any combiners for this generator.
    Optional.ofNullable( json.getJsonArray( COMBINERS_KEY))
      .map( combiners -> combiners.getValuesAs( JsonObject.class).stream())
      .orElse( Stream.empty())
      .forEach( combiner -> tupleGenerator.addCombiner( asCombiner( combiner)));

    return tupleGenerator;
    }

  /**
   * Returns the combiner represented by the given JSON object.
   */
  private TupleCombiner asCombiner( JsonObject json)
    {
    try
      {
      TupleCombiner tupleCombiner = new TupleCombiner();

      Optional.ofNullable( json.getJsonNumber( TUPLES_KEY))
        .ifPresent( n -> tupleCombiner.setTupleSize( n.intValue()));

      Optional.ofNullable( json.getJsonArray( INCLUDE_KEY))
        .map( includes -> includes.getValuesAs( JsonString.class).stream())
        .orElse( Stream.empty())
        .map( JsonString::getString)
        .forEach( var -> {
          try
            {
            try
              {
              tupleCombiner.addIncludedVar( var);
              }
            catch( Exception e)
              {
              throw new GeneratorSetException( e.getMessage());
              }
            }
          catch( Exception e)
            {
            throw new GeneratorSetException( "Error defining included variables", e);
            }
          });

      Optional.ofNullable( json.getJsonArray( EXCLUDE_KEY))
        .map( excludes -> excludes.getValuesAs( JsonString.class).stream())
        .orElse( Stream.empty())
        .map( JsonString::getString)
        .forEach( var -> {
          try
            {
            try
              {
              tupleCombiner.addExcludedVar( var);
              }
            catch( Exception e)
              {
              throw new GeneratorSetException( e.getMessage());
              }
            }
          catch( Exception e)
            {
            throw new GeneratorSetException( "Error defining excluded variables", e);
            }
          });

      Optional.ofNullable( json.getJsonArray( ONCE_KEY))
        .map( onceTuples -> onceTuples.getValuesAs( JsonObject.class).stream())
        .orElse( Stream.empty())
        .forEach( onceTuple -> {
          try
            {
            try
              {
              tupleCombiner.addOnceTuple( asTupleRef( onceTuple));
              }
            catch( Exception e)
              {
              throw new GeneratorSetException( e.getMessage());
              }
            }
          catch( Exception e)
            {
            throw new GeneratorSetException( "Error defining once tuples", e);
            }
          });


      return tupleCombiner;
      }
    catch( GeneratorSetException e)
      {
      throw new GeneratorSetException( "Error defining combiner", e);
      }
    }

  /**
   * Returns the TupleRef represented by the given JSON object.
   */
  private TupleRef asTupleRef( JsonObject json)
    {
    TupleRef tupleRef = new TupleRef();

    json.keySet().stream()
      .forEach( var -> {
        try
          {
          tupleRef.addVarBinding( new VarBinding( var, ObjectUtils.toObject( json.getString( var))));
          }
        catch( GeneratorSetException e)
          {
          throw new GeneratorSetException( String.format( "Error adding binding for variable=%s", var), e);
          }
        });

    return tupleRef;
    }

  /**
   * Reports a GeneratorSetException if the given string is not a valid function id. Otherwise, returns this string.
   */
  private String validFunction( String string)
    {
    return
      GeneratorSet.ALL.equals( string)
      ? string
      : validIdentifier( string);
    }

  /**
   * Reports a GeneratorSetException if the given string is not a valid identifier. Otherwise, returns this string.
   */
  private String validIdentifier( String string)
    {
    try
      {
      DefUtils.assertIdentifier( string);
      }
    catch( Exception e)
      {
      throw new GeneratorSetException( e.getMessage());
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
