//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.DefUtils;
import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.util.ObjectUtils;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonStructure;

/**
 * Converts between a {@link GeneratorSet} and its corresponding {@link JsonObject}.
 */
public final class GeneratorSetJson
  {
  private GeneratorSetJson()
    {
    // Static methods only
    }

  /**
   * Returns the IGeneratorSet represented by the given JSON object.
   */
  public static IGeneratorSet asGeneratorSet( JsonObject json)
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
  private static TupleGenerator asGenerator( JsonObject json)
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
  private static TupleCombiner asCombiner( JsonObject json)
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
  private static TupleRef asTupleRef( JsonObject json)
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
  private static String validFunction( String string)
    {
    return
      GeneratorSet.ALL.equals( string)
      ? string
      : validIdentifier( string);
    }

  /**
   * Reports a GeneratorSetException if the given string is not a valid identifier. Otherwise, returns this string.
   */
  private static String validIdentifier( String string)
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
   * Returns the JSON object that represents the given generator set.
   */
  public static JsonObject toJson( IGeneratorSet generatorSet)
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
  private static JsonStructure toJson( ITestCaseGenerator generator)
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
  private static JsonStructure toJson( TupleCombiner combiner)
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
  private static JsonStructure toJson( TupleRef tuple)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    toStream( tuple.getVarBindings()).forEach( binding -> builder.add( binding.getVar(), String.valueOf( binding.getValue())));
    
    return builder.build();
    }
  
  private static final String COMBINERS_KEY = "combiners";
  private static final String EXCLUDE_KEY = "exclude";
  private static final String INCLUDE_KEY = "include";
  private static final String ONCE_KEY = "once";
  private static final String SEED_KEY = "seed";
  private static final String TUPLES_KEY = "tuples";
  }
