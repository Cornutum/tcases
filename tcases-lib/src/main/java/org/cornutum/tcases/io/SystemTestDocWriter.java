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
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;

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

    xmlWriter_
      .element( TESTCASES_TAG)
      .attribute( SYSTEM_ATR, systemTest.getName())
      .content( () ->
        {
        writeAnnotations( systemTest);
        toStream( systemTest.getFunctionTestDefs()).forEach( this::writeFunction);
        })
      .write();
    }

  /**
   * Writes the given function test definition.
   */
  protected void writeFunction( FunctionTestDef function)
    {
    xmlWriter_
      .element( FUNCTION_TAG)
      .attribute( NAME_ATR, function.getName())
      .content( () ->
        {
        writeAnnotations( function);
        toStream( function.getTestCases())
          .sorted()
          .forEach( this::writeTestCase);
        })
      .write();
    }

  /**
   * Writes the given test case definition.
   */
  protected void writeTestCase( TestCase testCase)
    {
    xmlWriter_
      .element( TESTCASE_TAG)
      .attribute( ID_ATR, String.valueOf( testCase.getId()))
      .attributeIf( testCase.getType() == TestCase.Type.FAILURE, FAILURE_ATR, "true")
      .content( () ->
        {
        writeAnnotations( testCase);
        Arrays.stream( testCase.getVarTypes()).forEach( type -> writeInputs( testCase, type));
        })
      .write();
    }

  /**
   * Writes the input value definitions for all variables of the given type.
   */
  protected void writeInputs( TestCase testCase, String type)
    {
    xmlWriter_
      .element( INPUT_TAG)
      .attribute( TYPE_ATR, type)
      .content( () ->
        {
        toStream( testCase.getVarBindings( type))
          .sorted()
          .forEach( this::writeBinding);
        })
      .write();
    }

  /**
   * Writes the given variable input value definition.
   */
  protected void writeBinding( VarBinding binding)
    {
    xmlWriter_
      .element( VAR_TAG)
      .attribute( NAME_ATR, binding.getVar())
      .attributeIf( binding.isValueNA(), NA_ATR, "true")
      .attributeIf( !binding.isValueNA(), VALUE_ATR, String.valueOf( binding.getValue()))
      .attributeIf( !binding.isValueValid(), FAILURE_ATR, "true")
      .content( () -> writeAnnotations( binding))
      .write();
    }

  /**
   * Writes the given annotation definitions.
   */
  protected void writeAnnotations( Annotated annotated)
    {
    toStream( annotated.getAnnotations())
      .sorted()
      .forEach( annotation -> {
        xmlWriter_
          .element( HAS_TAG)
          .attribute( NAME_ATR, annotation)
          .attribute( VALUE_ATR, annotated.getAnnotation( annotation))
          .write();
        }); 
    }

  /**
   * Flushes the writer.
   */
  public void flush()
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
