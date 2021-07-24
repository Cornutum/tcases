//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.io.XmlWriter;

import static org.cornutum.tcases.generator.io.GeneratorSetDoc.*;
import static org.cornutum.tcases.generator.io.TupleGeneratorDoc.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;

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

    writer_
      .element( GENERATORS_TAG)
      .content( () ->
        {
        Arrays.stream( generatorSet.getGeneratorFunctions())
          .sorted()
          .forEach( function -> writeGenerator( function, generatorSet.getGenerator( function)));
        })
      .write();
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
    
    writer_
      .element( TUPLEGENERATOR_TAG)
      .attribute( FUNCTION_ATR, function)
      .attributeIf( seed != null, SEED_ATR, String.valueOf( seed))
      .attribute( TUPLES_ATR, String.valueOf( generator.getDefaultTupleSize()))
      .contentIf(
        !generator.getCombiners().isEmpty(),
        () -> generator.getCombiners().stream().forEach( combiner -> writeCombiner( combiner)))
      .write();
    }

  /**
   * Writes the given combiner definition.
   */
  protected void writeCombiner( TupleCombiner combiner)
    {
    writer_
      .element( COMBINE_TAG)
      .attribute( TUPLES_ATR, String.valueOf( combiner.getTupleSize()))
      .content( () ->
        {
        Arrays.stream( combiner.getIncluded())
          .sorted()
          .forEach( this::writeIncluded);
        
        Arrays.stream( combiner.getExcluded())
          .sorted()
          .forEach( this::writeExcluded);

        toStream( combiner.getOnceTuples()).forEach( this::writeOnceTuple);
        })
      .write();
    }

  /**
   * Writes the given once-only tuple definition.
   */
  protected void writeOnceTuple( TupleRef tuple)
    {
    writer_
      .element( ONCE_TAG)
      .content( () -> toStream( tuple.getVarBindings()).forEach( this::writeVarBinding))
      .write();
    }

  /**
   * Writes the given variable binding definition.
   */
  protected void writeVarBinding( VarBinding binding)
    {
    writer_
      .element( VAR_TAG)
      .attribute( NAME_ATR, binding.getVar())
      .attribute( VALUE_ATR, String.valueOf( binding.getValue()))
      .write();
    }

  /**
   * Writes the given included variable definition.
   */
  protected void writeIncluded( String var)
    {
    writer_
      .element( INCLUDE_TAG)
      .attribute( VAR_ATR, var)
      .write();
    }

  /**
   * Writes the given excluded variable definition.
   */
  protected void writeExcluded( String var)
    {
    writer_
      .element( EXCLUDE_TAG)
      .attribute( VAR_ATR, var)
      .write();
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
  @Override
  public void close() throws IOException
    {
    writer_.close();
    }

  private XmlWriter writer_;
  }
