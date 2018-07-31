//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.XmlWriter;
import static org.cornutum.tcases.generator.io.GeneratorSetDoc.*;
import static org.cornutum.tcases.generator.io.TupleGeneratorDoc.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;
import java.util.Optional;

/**
 * Writes a {@link IGeneratorSet} in the form of an XML document.
 *
 */
public class GeneratorSetDocWriter implements Closeable
  {
  /**
   * Creates a new GeneratorSetDocWriter object that writes to standard output.
   */
  public GeneratorSetDocWriter()
    {
    this( System.out);
    }
  
  /**
   * Creates a new GeneratorSetDocWriter object that writes to the given stream.
   */
  public GeneratorSetDocWriter( OutputStream stream)
    {
    this( new XmlWriter( stream));
    }
  
  /**
   * Creates a new GeneratorSetDocWriter object that writes to the given stream.
   */
  public GeneratorSetDocWriter( Writer writer)
    {
    this( new XmlWriter( writer));
    }
  
  /**
   * Creates a new GeneratorSetDocWriter object that writes to the given stream.
   */
  protected GeneratorSetDocWriter( XmlWriter writer)
    {
    writer_ = writer;
    }

  /**
   * Writes the given generator definition in the form of an XML document.
   */
  public void write( IGeneratorSet generatorSet)
    {
    writer_.writeDeclaration();

    writer_.writeElement(
      GENERATORS_TAG,
      () ->
        {
        Arrays.stream( generatorSet.getGeneratorFunctions())
          .sorted()
          .forEach( function -> writeGenerator( function, generatorSet.getGenerator( function)));
        });
    }

  /**
   * Writes the given generator definition.
   */
  protected void writeGenerator( String function, ITestCaseGenerator generator)
    {
    if( !generator.getClass().equals( TupleGenerator.class))
      {
      throw
        new UnsupportedOperationException
        ( "Can't write generator=" + generator
          + ": only TupleGenerator currently supported");
      }

    writeTupleGenerator( function, (TupleGenerator) generator);
    }

  /**
   * Writes the given generator definition.
   */
  protected void writeTupleGenerator( String function, TupleGenerator generator)
    {
    Long seed = generator.getRandomSeed();
    
    writer_.writeElement(
      TUPLEGENERATOR_TAG,

      MapBuilder
        .of( FUNCTION_ATR, function)
        .putIf( SEED_ATR, Optional.ofNullable( seed != null? String.valueOf( seed) : null))
        .put( TUPLES_ATR, String.valueOf( generator.getDefaultTupleSize()))
        .build(),
      
      () ->
        {
        for( TupleCombiner combiner : generator.getCombiners())
          {
          writeCombiner( combiner);
          }
        });
    }

  /**
   * Writes the given combiner definition.
   */
  protected void writeCombiner( TupleCombiner combiner)
    {
    writer_.writeElement(
      COMBINE_TAG,
      MapBuilder.of( TUPLES_ATR, String.valueOf( combiner.getTupleSize())).build(),
      () ->
        {
        Arrays.stream( combiner.getIncluded())
          .sorted()
          .forEach( included -> writeIncluded( included));
        
        Arrays.stream( combiner.getExcluded())
          .sorted()
          .forEach( excluded -> writeExcluded( excluded));

        toStream( combiner.getOnceTuples()).forEach( tuple -> writeOnceTuple( tuple));
        });
    }

  /**
   * Writes the given once-only tuple definition.
   */
  protected void writeOnceTuple( TupleRef tuple)
    {
    writer_.writeElement(
      ONCE_TAG,
      () ->
        {
        toStream( tuple.getVarBindings()).forEach( varBinding -> writeVarBinding( varBinding));
        });
    }

  /**
   * Writes the given variable binding definition.
   */
  protected void writeVarBinding( VarBinding binding)
    {
    writer_.writeElement(
      VAR_TAG,
      MapBuilder.of( NAME_ATR, binding.getVar()).put( VALUE_ATR, String.valueOf( binding.getValue())).build());
    }

  /**
   * Writes the given included variable definition.
   */
  protected void writeIncluded( String var)
    {
    writer_.writeElement(
      INCLUDE_TAG,
      MapBuilder.of( VAR_ATR, var).build());
    }

  /**
   * Writes the given excluded variable definition.
   */
  protected void writeExcluded( String var)
    {
    writer_.writeElement(
      EXCLUDE_TAG,
      MapBuilder.of( VAR_ATR, var).build());
    }

  /**
   * Flushes the writer.
   */
  public void flush()
    {
    writer_.flush();
    }

  /**
   * Closes the writer.
   */
  public void close() throws IOException
    {
    writer_.close();
    }

  private XmlWriter writer_;
  }
