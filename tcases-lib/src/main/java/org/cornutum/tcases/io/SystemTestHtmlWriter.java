//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.XmlWriter;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Predicate;
import org.apache.commons.collections4.iterators.PeekingIterator;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Writer;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

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
    xmlWriter_.writeElement(
      "HTML",
      () ->
        {
        xmlWriter_.writeElement(
          "HEAD",
          () ->
            {
            xmlWriter_.writeElement( "TITLE", "Test Cases: " + systemTest.getName());

            if( defaultStyle)
              {
              writeDefaultStyle();
              }
            else if( stylesheet != null)
              {
              xmlWriter_.writeElement(
                "LINK",
                MapBuilder
                  .of( "rel", "stylesheet")
                  .put( "type", "text/css")
                  .put( "href", String.valueOf( stylesheet))
                  .build());
              }
            });

        xmlWriter_.writeElement(
          "BODY",
          () ->
            {
            toStream( systemTest.getFunctionTestDefs()).forEach( function -> writeFunction( function));
            if( defaultStyle)
              {
              writeDefaultScript();
              }
            else if( script != null)
              {
              xmlWriter_.writeElement(
                "SCRIPT",
                MapBuilder.of( "src", String.valueOf( script)).put( "type", "text/javascript").build());
              }
            });
        });
    }

  /**
   * Writes the given function test definition.
   */
  protected void writeFunction( FunctionTestDef function)
    {
    xmlWriter_.writeElement(
      "DIV",
      MapBuilder.of( "id", function.getName()).put( "class", "function").build(),
      () ->
        {
        xmlWriter_.writeElement( "H1", function.getName());
        writeTestCases( function);
        });
    }

  /**
   * Writes the test cases for given function test definition.
   */
  protected void writeTestCases( FunctionTestDef function)
    {
    toStream( function.getTestCases())
      .forEach( testCase ->
          {
          String testCaseType = testCase.getType()==TestCase.Type.FAILURE? "failure" : "success";

          xmlWriter_.writeElement(
            "DIV",

            MapBuilder
              .of( "id", function.getName() + "." + testCase.getId())
              .put( "class", "testCase " + testCaseType)
              .build(),
            
            () ->
              {
              xmlWriter_.writeElement( "H2", "Test Case " + testCase.getId());
              Arrays.stream( testCase.getVarTypes()).forEach( type -> writeInputs( testCase, type));
              });
          });
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
      xmlWriter_.writeElement(
        "DIV",
        MapBuilder.of( "class", "input " + type).build(),
        () ->
          {
          if( !IVarDef.ARG.equals( type))
            {
            xmlWriter_.writeElement( "H3", type);
            }
    
          writeVarSets( 0, varBindings);
          });
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
        writeBinding( getVar( varBinding), String.valueOf( varBinding.getValue()), varBinding.isValueValid());
        }
      }
    else
      {
      xmlWriter_.writeElement(
        "TR",
        () ->
          {
          xmlWriter_.writeElement( "TH", varSet);

          xmlWriter_.writeElement(
            "TD",
            () ->
              {
              writeVarSets( varSetLevel + 1, varBindings);
              });
          });
      }
    }

  /**
   * Writes the definitions of all variable sets defined by the given bindings.
   */
  protected void writeVarSets( int varSetLevel, Iterator<VarBinding> varBindings)
    {
    xmlWriter_.writeElement(
      "TABLE",
      MapBuilder.of( "class", varSetLevel % 2 == 0? "light" : "dark").build(),
      () ->
        {
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
        });
    }

  /**
   * Writes the given input variable binding.
   */
  protected void writeBinding( String var, String value, boolean isValid)
    {
    xmlWriter_.writeElement(
      "TR",
      MapBuilder.optionalOf( "class", Optional.ofNullable( !isValid? "failure" : null)),
      Optional.of( () ->
        {
        xmlWriter_.writeElement( "TH", var);
        xmlWriter_.writeElement( "TD", value);
        }));
    }

  /**
   * Writes the default system test style definition.
   */
  protected void writeDefaultStyle()
    {
    xmlWriter_.writeElement(
      "STYLE",
      () ->
        {
        try( BufferedReader in = new BufferedReader( new InputStreamReader( getClass().getResourceAsStream( "system-test.css"), "UTF-8")))
          {
          String line;
          while(( line = in.readLine()) != null)
            {
            xmlWriter_.println( line);
            }
          }
        catch( Exception e)
          {
          throw new RuntimeException( "Can't write resource=system-test.css", e);
          }
        });
    }

  /**
   * Writes the default system test presentation script.
   */
  protected void writeDefaultScript()
    {
    xmlWriter_.writeElement(
      "SCRIPT",
      () ->
        {
        try( BufferedReader in = new BufferedReader( new InputStreamReader( getClass().getResourceAsStream( "system-test.js"), "UTF-8"));)
          {
          String line;
          while(( line = in.readLine()) != null)
            {
            xmlWriter_.println( line);
            }
          }
        catch( Exception e)
          {
          throw new RuntimeException( "Can't write resource=system-test.js", e);
          }
        });
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
