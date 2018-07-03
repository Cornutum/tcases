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

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;
import java.util.Iterator;

/**
 * Writes a {@link SystemTestDef} in the form of an XML document.
 *
 */
public class SystemTestDocWriter extends AbstractSystemTestWriter
  {
  /**
   * Creates a new SystemTestDocWriter object that writes to standard output.
   */
  public SystemTestDocWriter()
    {
    super();
    }
  
  /**
   * Creates a new SystemTestDocWriter object that writes to the given stream.
   */
  public SystemTestDocWriter( OutputStream stream)
    {
    super( stream);
    }
  
  /**
   * Creates a new SystemTestDocWriter object that writes to the given stream.
   */
  public SystemTestDocWriter( Writer writer)
    {
    super( writer);
    }

  /**
   * Writes the given system test definition the form of an XML document.
   */
  public void write( SystemTestDef systemTest)
    {
    xmlWriter_.writeDeclaration();

    xmlWriter_.writeTagStart( TESTCASES_TAG);
    xmlWriter_.writeAttribute( SYSTEM_ATR, systemTest.getName());
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    writeAnnotations( systemTest);
    for( Iterator<FunctionTestDef> functions =systemTest.getFunctionTestDefs();
         functions.hasNext();)
      {
      writeFunction( functions.next());
      }
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( TESTCASES_TAG);
    }

  /**
   * Writes the given function test definition.
   */
  protected void writeFunction( FunctionTestDef function)
    {
    xmlWriter_.writeTagStart( FUNCTION_TAG);
    xmlWriter_.writeAttribute( NAME_ATR, function.getName());
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    writeAnnotations( function);
    TestCase[] testCases = IteratorUtils.toArray( function.getTestCases(), TestCase.class);
    Arrays.sort( testCases);
    for( int i = 0; i < testCases.length; i++)
      {
      writeTestCase( testCases[i]);
      }
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( FUNCTION_TAG);
    }

  /**
   * Writes the given test case definition.
   */
  protected void writeTestCase( TestCase testCase)
    {
    xmlWriter_.writeTagStart( TESTCASE_TAG);
    xmlWriter_.writeAttribute( ID_ATR, String.valueOf( testCase.getId()));
    if( testCase.getType() == TestCase.Type.FAILURE)
      {
      xmlWriter_.writeAttribute( FAILURE_ATR, "true");
      }
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    writeAnnotations( testCase);
    String[] types = testCase.getVarTypes();
    for( int i = 0; i < types.length; i++)
      {
      writeInputs( testCase, types[i]);
      }
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( TESTCASE_TAG);
    }

  /**
   * Writes the input value definitions for all variables of the given type.
   */
  protected void writeInputs( TestCase testCase, String type)
    {
    xmlWriter_.writeTagStart( INPUT_TAG);
    xmlWriter_.writeAttribute( TYPE_ATR, type);
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    VarBinding[] bindings = IteratorUtils.toArray( testCase.getVarBindings( type), VarBinding.class);
    Arrays.sort( bindings);
    for( int i = 0; i < bindings.length; i++)
      {
      writeBinding( bindings[i]);
      }
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( INPUT_TAG);
    }

  /**
   * Writes the given variable input value definition.
   */
  protected void writeBinding( VarBinding binding)
    {
    xmlWriter_.writeTagStart( VAR_TAG);
    xmlWriter_.writeAttribute( NAME_ATR, binding.getVar());
    if( binding.isValueNA())
      {
      xmlWriter_.writeAttribute( NA_ATR, "true");
      }
    else
      {
      xmlWriter_.writeAttribute( VALUE_ATR, binding.getValue());
      }
    if( !binding.isValueValid())
      {
      xmlWriter_.writeAttribute( FAILURE_ATR, "true");
      }

    if( binding.getAnnotationCount() == 0)
      {
      xmlWriter_.writeEmptyElementEnd();
      }
    else
      {
      xmlWriter_.writeTagEnd();
      xmlWriter_.indent();
      writeAnnotations( binding);
      xmlWriter_.unindent();
      xmlWriter_.writeElementEnd( VAR_TAG);
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
      xmlWriter_.writeTagStart( HAS_TAG);
      xmlWriter_.writeAttribute( NAME_ATR, annotations[i]);
      xmlWriter_.writeAttribute( VALUE_ATR, annotated.getAnnotation( annotations[i]));
      xmlWriter_.writeEmptyElementEnd();
      } 
    }

  /**
   * Flushes the writer.
   */
  public void flush() throws IOException
    {
    getXmlWriter().flush();
    }

  /**
   * Closes the writer.
   */
  public void close() throws IOException
    {
    getXmlWriter().close();
    }

  /**
   * Changes the output stream for this writer.
   */
  protected void setWriter( Writer writer)
    {
    setXmlWriter( new XmlWriter( writer));
    }

  /**
   * Changes the XmlWriter for this writer.
   */
  private void setXmlWriter( XmlWriter xmlWriter)
    {
    xmlWriter_ = xmlWriter;
    }

  /**
   * Returns the XmlWriter for this writer.
   */
  private XmlWriter getXmlWriter()
    {
    return xmlWriter_;
    }

  private XmlWriter xmlWriter_;
  }
