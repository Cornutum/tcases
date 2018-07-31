//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.XmlWriter;
import static org.cornutum.tcases.io.SystemTestDoc.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Arrays;
import java.util.Optional;

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

    xmlWriter_.writeElement(
      TESTCASES_TAG,
      MapBuilder.of( SYSTEM_ATR, systemTest.getName()).build(),
      () ->
        {
        writeAnnotations( systemTest);
        toStream( systemTest.getFunctionTestDefs()).forEach( function -> writeFunction( function));
        });
    }

  /**
   * Writes the given function test definition.
   */
  protected void writeFunction( FunctionTestDef function)
    {
    xmlWriter_.writeElement(
      FUNCTION_TAG,
      MapBuilder.of( NAME_ATR, function.getName()).build(),
      () ->
        {
        writeAnnotations( function);
        toStream( function.getTestCases())
          .sorted()
          .forEach( testCase -> writeTestCase( testCase));
        });
    }

  /**
   * Writes the given test case definition.
   */
  protected void writeTestCase( TestCase testCase)
    {
    xmlWriter_.writeElement(
      TESTCASE_TAG,

      MapBuilder
        .of( ID_ATR, String.valueOf( testCase.getId()))
        .putIf( FAILURE_ATR, Optional.ofNullable( testCase.getType() == TestCase.Type.FAILURE? "true" : null))
        .build(),

      () ->
        {
        writeAnnotations( testCase);
        Arrays.stream( testCase.getVarTypes()).forEach( type -> writeInputs( testCase, type));
        });
    }

  /**
   * Writes the input value definitions for all variables of the given type.
   */
  protected void writeInputs( TestCase testCase, String type)
    {
    xmlWriter_.writeElement(
      INPUT_TAG,
      MapBuilder.of( TYPE_ATR, type).build(),
      () ->
        {
        toStream( testCase.getVarBindings( type))
          .sorted()
          .forEach( binding -> writeBinding( binding));
        });
    }

  /**
   * Writes the given variable input value definition.
   */
  protected void writeBinding( VarBinding binding)
    {
    xmlWriter_.writeElement(
      VAR_TAG,
      MapBuilder
        .of( NAME_ATR, binding.getVar())
        .putIf( NA_ATR, Optional.ofNullable( binding.isValueNA()? "true" : null))
        .putIf( VALUE_ATR, Optional.ofNullable( binding.isValueNA()? null : String.valueOf( binding.getValue())))
        .putIf( FAILURE_ATR, Optional.ofNullable( binding.isValueValid()? null : "true"))
        .build(),
      () ->
        {
        writeAnnotations( binding);
        });
    }

  /**
   * Writes the given annotation definitions.
   */
  protected void writeAnnotations( Annotated annotated)
    {
    toStream( annotated.getAnnotations())
      .sorted()
      .forEach( annotation -> {
        xmlWriter_.writeElement(
          HAS_TAG,
          MapBuilder.of( NAME_ATR, annotation).put( VALUE_ATR, annotated.getAnnotation( annotation)).build());
        }); 
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
