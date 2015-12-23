//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.XmlWriter;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Predicate;
import org.apache.commons.collections4.iterators.PeekingIterator;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Writes a {@link SystemTestDef} in the form of an HTML document.
 *
 */
public class SystemTestHtmlWriter extends AbstractSystemTestWriter
  {
  /**
   * Creates a new SystemTestHtmlWriter object that writes to standard output.
   */
  public SystemTestHtmlWriter()
    {
    super();
    }
  
  /**
   * Creates a new SystemTestHtmlWriter object that writes to the given stream.
   */
  public SystemTestHtmlWriter( OutputStream stream)
    {
    super( stream);
    }
  
  /**
   * Creates a new SystemTestHtmlWriter object that writes to the given stream.
   */
  public SystemTestHtmlWriter( Writer writer)
    {
    super( writer);
    }

  /**
   * Writes the given system test definition the form of an HTML document.
   * If a stylesheet URI is defined, includes a link to this stylesheet resource.
   * Otherwise, no style is defined for this HTML document.
   * If a JavaScript URI is defined, includes a link to this script resource.
   * Otherwise, no JavaScript is defined for this HTML document.
   */
  public void write( SystemTestDef systemTest, URI stylesheet, URI script)
    {
    write( systemTest, false, stylesheet, script);
    }

  /**
   * Writes the given system test definition the form of an HTML document.
   */
  public void write( SystemTestDef systemTest)
    {
    write( systemTest, true, null, null);
    }

  /**
   * Writes the given system test definition the form of an HTML document.
   * If <CODE>defaultStyle</CODE> is true, uses the default stylesheet and ignores the given <CODE>stylesheet</CODE>
   * and <CODE>script</CODE> parameters.
   * Otherwise, if a stylesheet URI is defined, includes a link to this stylesheet resource and if a
   * JavaScript URI is defined, includes a link to this script resource.
   */
  public void write( SystemTestDef systemTest, boolean defaultStyle, URI stylesheet, URI script)
    {
    xmlWriter_.writeElementStart( "HTML");
    xmlWriter_.indent();

    xmlWriter_.writeElementStart( "HEAD");
    xmlWriter_.indent();

    xmlWriter_.writeElement( "TITLE", "Test Cases: " + systemTest.getName());

    if( defaultStyle)
      {
      writeDefaultStyle();
      }
    else if( stylesheet != null)
      {
      xmlWriter_.writeTagStart( "LINK");
      xmlWriter_.writeAttribute( "rel", "stylesheet");
      xmlWriter_.writeAttribute( "type", "text/css");
      xmlWriter_.writeAttribute( "href", String.valueOf( stylesheet));
      xmlWriter_.writeEmptyElementEnd();
      }
    
    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "HEAD");
    
    xmlWriter_.writeElementStart( "BODY");
    xmlWriter_.indent();
    
    for( Iterator<FunctionTestDef> functions =systemTest.getFunctionTestDefs(); functions.hasNext();)
      {
      writeFunction( functions.next());
      }

    if( defaultStyle)
      {
      writeDefaultScript();
      }
    else if( script != null)
      {
      xmlWriter_.writeTagStart( "SCRIPT");
      xmlWriter_.writeAttribute( "src", String.valueOf( script));
      xmlWriter_.writeAttribute( "type", "text/javascript");
      xmlWriter_.writeTagEnd();
      xmlWriter_.writeElementEnd( "SCRIPT");
      }
    
    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "BODY");

    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "HTML");
    }

  /**
   * Writes the given function test definition.
   */
  protected void writeFunction( FunctionTestDef function)
    {
    xmlWriter_.writeTagStart( "DIV");
    xmlWriter_.writeAttribute( "id", function.getName());
    xmlWriter_.writeAttribute( "class", "function");
    xmlWriter_.writeTagEnd();
    xmlWriter_.indent();

    xmlWriter_.writeElement( "H1", function.getName());

    writeTestCases( function);
    
    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "DIV");
    }

  /**
   * Writes the test cases for given function test definition.
   */
  protected void writeTestCases( FunctionTestDef function)
    {
    for( Iterator<TestCase> testCases = function.getTestCases(); testCases.hasNext();)
      {
      TestCase testCase = testCases.next();
      String testCaseType = testCase.getType()==TestCase.Type.FAILURE? "failure" : "success";
      
      xmlWriter_.writeTagStart( "DIV");
      xmlWriter_.writeAttribute( "id", function.getName() + "." + testCase.getId());
      xmlWriter_.writeAttribute( "class", "testCase " + testCaseType);
      xmlWriter_.writeTagEnd();
      xmlWriter_.indent();

      xmlWriter_.writeElement( "H2", "Test Case " + testCase.getId());
      
      String[] types = testCase.getVarTypes();
      for( int i = 0; i < types.length; i++)
        {
        writeInputs( testCase, types[i]);
        }
      
      xmlWriter_.unindent();
      xmlWriter_.writeElementEnd( "DIV");
      }
    }

  /**
   * Writes the input value definitions for all variables of the given type.
   */
  protected void writeInputs( TestCase testCase, String type)
    {
    Iterator<VarBinding> varBindings =
      IteratorUtils.filteredIterator
        ( testCase.getVarBindings( type),
          new Predicate<VarBinding>()
             {
             public boolean evaluate( VarBinding binding)
                {
                return !binding.isValueNA();
                }
            });

    if( varBindings.hasNext())
      {
      xmlWriter_.writeTagStart( "DIV");
      xmlWriter_.writeAttribute( "class", "input " + type);
      xmlWriter_.writeTagEnd();
      xmlWriter_.indent();

      if( !IVarDef.ARG.equals( type))
        {
        xmlWriter_.writeElement( "H3", type);
        }
    
      writeVarSets( 0, varBindings);
    
      xmlWriter_.unindent();
      xmlWriter_.writeElementEnd( "DIV");
      }
    }

  /**
   * Writes the input value definitions for the given variables.
   */
  protected void writeVarSet( String varSet, int varSetLevel, Iterator<VarBinding> varBindings)
    {
    if( "".equals( varSet))
      {
      while( varBindings.hasNext())
        {
        VarBinding varBinding = varBindings.next();
        writeBinding( getVar( varBinding), varBinding.getValue(), varBinding.isValueValid());
        }
      }
    else
      {
      xmlWriter_.writeElementStart( "TR");
      xmlWriter_.indent();

      xmlWriter_.writeElement( "TH", varSet);

      xmlWriter_.writeElementStart( "TD");
      xmlWriter_.indent();

      writeVarSets( varSetLevel + 1, varBindings);
        
      xmlWriter_.unindent();
      xmlWriter_.writeElementEnd( "TD");

      xmlWriter_.unindent();
      xmlWriter_.writeElementEnd( "TR");
      }
    }

  /**
   * Writes the definitions of all variable sets defined by the given bindings.
   */
  protected void writeVarSets( int varSetLevel, Iterator<VarBinding> varBindings)
    {
    xmlWriter_.writeTagStart( "TABLE");
    xmlWriter_.writeAttribute( "class", varSetLevel % 2 == 0? "light" : "dark");
    xmlWriter_.writeTagEnd();
    xmlWriter_.indent();
    
    String varSet = "";
    String varSetNext = "";
    for( PeekingIterator<VarBinding> peekBindings = new PeekingIterator<VarBinding>( varBindings);
         peekBindings.hasNext();
         varSet = varSetNext)
      {
      List<VarBinding> varSetBindings = new ArrayList<VarBinding>();
      for( varSetNext = "";

           peekBindings.hasNext()
             && (varSetNext = getVarSet( varSetLevel, peekBindings.peek())).equals( varSet);

           varSetBindings.add( peekBindings.next()));

      writeVarSet( varSet, varSetLevel, varSetBindings.iterator());
      }

    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "TABLE");
    }

  /**
   * Writes the given input variable binding.
   */
  protected void writeBinding( String var, String value, boolean isValid)
    {
    xmlWriter_.writeTagStart( "TR");
    if( !isValid)
      {
      xmlWriter_.writeAttribute( "class", "failure");
      }
    xmlWriter_.writeTagEnd();
    xmlWriter_.indent();

    xmlWriter_.writeElement( "TH", var);
    xmlWriter_.writeElement( "TD", value);

    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "TR");
    }

  /**
   * Writes the default system test style definition.
   */
  protected void writeDefaultStyle()
    {
    xmlWriter_.writeElementStart( "STYLE");
    xmlWriter_.indent();

    LineIterator styleLines = null;
    try
      {
      for( styleLines = IOUtils.lineIterator( getClass().getResourceAsStream( "system-test.css"), "UTF-8");
           styleLines.hasNext();
           xmlWriter_.println( styleLines.next()));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write resource=system-test.css", e);
      }
    finally
      {
      LineIterator.closeQuietly( styleLines);
      }
    
    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "STYLE");
    }

  /**
   * Writes the default system test presentation script.
   */
  protected void writeDefaultScript()
    {
    xmlWriter_.writeElementStart( "SCRIPT");
    xmlWriter_.indent();

    LineIterator scriptLines = null;
    try
      {
      for( scriptLines = IOUtils.lineIterator( getClass().getResourceAsStream( "system-test.js"), "UTF-8");
           scriptLines.hasNext();
           xmlWriter_.println( scriptLines.next()));
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write resource=system-test.js", e);
      }
    finally
      {
      LineIterator.closeQuietly( scriptLines);
      }
    
    xmlWriter_.unindent();
    xmlWriter_.writeElementEnd( "SCRIPT");
    }

  /**
   * Returns the variable name for the given variable binding.
   */
  private String getVar( VarBinding varBinding)
    {
    String[] varPathNames = StringUtils.split( varBinding.getVar(), '.');
    return varPathNames[ varPathNames.length - 1];
    }

  /**
   * Returns variable set name at the specified level of the given variable binding.
   * Returns an empty string if no such variable set is defined.
   */
  private String getVarSet( int varSetLevel, VarBinding varBinding)
    {
    String[] varPathNames = StringUtils.split( varBinding.getVar(), '.');
    return
      varSetLevel < varPathNames.length - 1
      ? varPathNames[ varSetLevel]
      : "";
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
