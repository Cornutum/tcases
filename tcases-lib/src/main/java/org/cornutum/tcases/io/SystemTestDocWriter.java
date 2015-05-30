//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.XmlWriter;
import static org.cornutum.tcases.io.SystemTestDoc.*;

import org.apache.commons.collections4.IteratorUtils;

import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;
import java.util.Iterator;

/**
 * Writes a {@link SystemTestDef} in the form of an XML document.
 *
 */
public class SystemTestDocWriter implements Closeable
  {
  /**
   * Creates a new SystemTestDocWriter object that writes to standard output.
   */
  public SystemTestDocWriter()
    {
    this( System.out);
    }
  
  /**
   * Creates a new SystemTestDocWriter object that writes to the given stream.
   */
  public SystemTestDocWriter( OutputStream stream)
    {
    this( new XmlWriter( stream));
    }
  
  /**
   * Creates a new SystemTestDocWriter object that writes to the given stream.
   */
  public SystemTestDocWriter( Writer writer)
    {
    this( new XmlWriter( writer));
    }
  
  /**
   * Creates a new SystemTestDocWriter object that writes to the given stream.
   */
  protected SystemTestDocWriter( XmlWriter writer)
    {
    writer_ = writer;
    }

  /**
   * Writes the given system test definition the form of an XML document.
   */
  public void write( SystemTestDef systemTest)
    {
    writer_.writeDeclaration();

    writer_.writeTagStart( TESTCASES_TAG);
    writer_.writeAttribute( SYSTEM_ATR, systemTest.getName());
    writer_.writeTagEnd();

    writer_.indent();
    for( Iterator<FunctionTestDef> functions =systemTest.getFunctionTestDefs();
         functions.hasNext();)
      {
      writeFunction( functions.next());
      }
    writer_.unindent();
    
    writer_.writeElementEnd( TESTCASES_TAG);
    }

  /**
   * Writes the given function test definition.
   */
  protected void writeFunction( FunctionTestDef function)
    {
    writer_.writeTagStart( FUNCTION_TAG);
    writer_.writeAttribute( NAME_ATR, function.getName());
    writer_.writeTagEnd();

    writer_.indent();
    TestCase[] testCases = IteratorUtils.toArray( function.getTestCases(), TestCase.class);
    Arrays.sort( testCases);
    for( int i = 0; i < testCases.length; i++)
      {
      writeTestCase( testCases[i]);
      }
    writer_.unindent();
    
    writer_.writeElementEnd( FUNCTION_TAG);
    }

  /**
   * Writes the given test case definition.
   */
  protected void writeTestCase( TestCase testCase)
    {
    writer_.writeTagStart( TESTCASE_TAG);
    writer_.writeAttribute( ID_ATR, String.valueOf( testCase.getId()));
    if( testCase.getType() == TestCase.Type.FAILURE)
      {
      writer_.writeAttribute( FAILURE_ATR, "true");
      }
    writer_.writeTagEnd();

    writer_.indent();
    writeAnnotations( testCase);
    String[] types = testCase.getVarTypes();
    for( int i = 0; i < types.length; i++)
      {
      writeInputs( testCase, types[i]);
      }
    writer_.unindent();
    
    writer_.writeElementEnd( TESTCASE_TAG);
    }

  /**
   * Writes the input value definitions for all variables of the given type.
   */
  protected void writeInputs( TestCase testCase, String type)
    {
    writer_.writeTagStart( INPUT_TAG);
    writer_.writeAttribute( TYPE_ATR, type);
    writer_.writeTagEnd();

    writer_.indent();
    VarBinding[] bindings = IteratorUtils.toArray( testCase.getVarBindings( type), VarBinding.class);
    Arrays.sort( bindings);
    for( int i = 0; i < bindings.length; i++)
      {
      writeBinding( bindings[i]);
      }
    writer_.unindent();
    
    writer_.writeElementEnd( INPUT_TAG);
    }

  /**
   * Writes the given variable input value definition.
   */
  protected void writeBinding( VarBinding binding)
    {
    writer_.writeTagStart( VAR_TAG);
    writer_.writeAttribute( NAME_ATR, binding.getVar());
    writer_.writeAttribute( VALUE_ATR, binding.getValue());
    if( !binding.isValueValid())
      {
      writer_.writeAttribute( FAILURE_ATR, "true");
      }

    if( binding.getAnnotationCount() == 0)
      {
      writer_.writeEmptyElementEnd();
      }
    else
      {
      writer_.writeTagEnd();
      writer_.indent();
      writeAnnotations( binding);
      writer_.unindent();
      writer_.writeElementEnd( VAR_TAG);
      }
    }

  /**
   * Writes the given annotation definitions.
   */
  protected void writeAnnotations( Annotated annotated)
    {
    String[] annotations = IteratorUtils.toArray( annotated.getAnnotations(), String.class);
    Arrays.sort( annotations);
    for( int i = 0; i < annotations.length; i++)
      {
      writer_.writeTagStart( HAS_TAG);
      writer_.writeAttribute( NAME_ATR, annotations[i]);
      writer_.writeAttribute( VALUE_ATR, annotated.getAnnotation( annotations[i]));
      writer_.writeEmptyElementEnd();
      } 
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
