//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.util.XmlWriter;
import static org.cornutum.tcases.generator.io.GeneratorSetDoc.*;
import static org.cornutum.tcases.generator.io.TupleGeneratorDoc.*;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;
import java.util.Iterator;

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

    for( Iterator<TupleRef> onceTuples = combiner.getOnceTuples(); onceTuples.hasNext(); )
      {
      writeOnceTuple( onceTuples.next());
      }
    
    writer_.unindent();
    
    writer_.writeElementEnd( COMBINE_TAG);
    }

  /**
   * Writes the given once-only tuple definition.
   */
  protected void writeOnceTuple( TupleRef tuple)
    {
    writer_.writeElementStart( ONCE_TAG);
    writer_.indent();
    for( Iterator<VarBinding> varBindings = tuple.getVarBindings(); varBindings.hasNext(); )
      {
      writeVarBinding( varBindings.next());
      }
    writer_.unindent();
    writer_.writeElementEnd( ONCE_TAG);
    }

  /**
   * Writes the given variable binding definition.
   */
  protected void writeVarBinding( VarBinding binding)
    {
    writer_.writeTagStart( VAR_TAG);
    writer_.writeAttribute( NAME_ATR, binding.getVar());      
    writer_.writeAttribute( VALUE_ATR, binding.getValue());      
    writer_.writeEmptyElementEnd();
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
  public void close() throws IOException
    {
    writer_.close();
    }

  private XmlWriter writer_;
  }
