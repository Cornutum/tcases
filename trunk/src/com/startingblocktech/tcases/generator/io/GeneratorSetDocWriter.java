//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator.io;

import com.startingblocktech.tcases.generator.*;
import com.startingblocktech.tcases.util.XmlWriter;
import static com.startingblocktech.tcases.generator.io.GeneratorSetDoc.*;
import static com.startingblocktech.tcases.generator.io.TupleGeneratorDoc.*;

import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;

/**
 * Writes a {@link IGeneratorSet} in the form of an XML document.
 *
 * @version $Revision$, $Date$
 */
public class GeneratorSetDocWriter
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

    writer_.writeElementStart( GENERATORS_TAG);

    writer_.indent();
    String[] functions = generatorSet.getGeneratorFunctions();
    Arrays.sort( functions);
    for( int i = 0; i < functions.length; i++)
      {
      writeGenerator( functions[i], generatorSet.getGenerator( functions[i]));
      }
    writer_.unindent();
    
    writer_.writeElementEnd( GENERATORS_TAG);
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
    writer_.writeTagStart( TUPLEGENERATOR_TAG);
    writer_.writeAttribute( FUNCTION_ATR, function);

    Long seed = generator.getRandomSeed();
    if( seed != null)
      {
      writer_.writeAttribute( SEED_ATR, String.valueOf( seed));      
      }

    writer_.writeAttribute( TUPLES_ATR, String.valueOf( generator.getDefaultTupleSize()));      
    writer_.writeTagEnd();

    writer_.indent();
    for( TupleCombiner combiner : generator.getCombiners())
      {
      writeCombiner( combiner);
      }
    writer_.unindent();
    
    writer_.writeElementEnd( TUPLEGENERATOR_TAG);
    }

  /**
   * Writes the given combiner definition.
   */
  protected void writeCombiner( TupleCombiner combiner)
    {
    writer_.writeTagStart( COMBINE_TAG);
    writer_.writeAttribute( TUPLES_ATR, String.valueOf( combiner.getTupleSize()));      
    writer_.writeTagEnd();

    writer_.indent();
    
    String[] included = combiner.getIncluded();
    Arrays.sort( included);
    for( int i = 0; i < included.length; i++)
      {
      writeIncluded( included[i]);
      }
    
    String[] excluded = combiner.getExcluded();
    Arrays.sort( excluded);
    for( int i = 0; i < excluded.length; i++)
      {
      writeExcluded( excluded[i]);
      }
    
    writer_.unindent();
    
    writer_.writeElementEnd( COMBINE_TAG);
    }

  /**
   * Writes the given included variable definition.
   */
  protected void writeIncluded( String var)
    {
    writer_.writeTagStart( INCLUDE_TAG);
    writer_.writeAttribute( VAR_ATR, var);      
    writer_.writeEmptyElementEnd();
    }

  /**
   * Writes the given excluded variable definition.
   */
  protected void writeExcluded( String var)
    {
    writer_.writeTagStart( EXCLUDE_TAG);
    writer_.writeAttribute( VAR_ATR, var);      
    writer_.writeEmptyElementEnd();
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
  public void close()
    {
    writer_.close();
    }

  private XmlWriter writer_;
  }
