//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.*;
import org.cornutum.tcases.generator.io.*;
import org.cornutum.tcases.io.*;

import org.apache.commons.io.IOUtils;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Generates a set of {@link TestCase test cases} from XML documents that define the {@link SystemInputDef system input model},
 * {@link IGeneratorSet generators}, and {@link SystemTestDef base tests} for a Tcases {@link Project project}.
 */
public class TcasesIO
  {  
  /**
   * Creates a new TcasesIO object.
   */
  private TcasesIO()
    {
    // Static methods only
    }

  /**
   * Returns test case definitions for the given {@link SystemInputDocReader system input definition}, using the given 
   * {@link GeneratorSetDocReader generator set} and {@link SystemTestDocReader base test definitions}.
   * If <CODE>genDefStream</CODE> is null, the default generator is
   * used.  If <CODE>baseDefStream</CODE> is null, no base test definitions are used.
   */
  public static SystemTestDef getTests( InputStream inputDefStream, InputStream genDefStream, InputStream baseDefStream)
    {
    SystemInputDocReader inputDefReader = null;
    GeneratorSetDocReader genDefReader = null;
    SystemTestDocReader baseDefReader = null;

    try
      {
      inputDefReader = new SystemInputDocReader( inputDefStream);
      genDefReader = genDefStream==null? null : new GeneratorSetDocReader( genDefStream);
      baseDefReader = baseDefStream==null? null : new SystemTestDocReader( baseDefStream);

      return
        Tcases.getTests
        ( inputDefReader.getSystemInputDef(),
          genDefReader==null? null : genDefReader.getGeneratorSet(),
          baseDefReader==null? null : baseDefReader.getSystemTestDef());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get test definitions", e);
      }
    finally
      {
      IOUtils.closeQuietly( inputDefReader, null);
      IOUtils.closeQuietly( genDefReader, null);
      IOUtils.closeQuietly( baseDefReader, null);
      }
    }

  /**
   * Returns new test case definitions for the given {@link SystemInputDocReader system input definition}, using the default generator.
   */
  public static SystemTestDef getTests( InputStream inputDefStream)
    {
    return getTests( inputDefStream, null, null);
    }

  /**
   * Writes an XML document describing the given system input definition to the given output stream.
   */
  public static void writeInputModel( SystemInputDef inputDef, OutputStream outputStream)
    {
    try( SystemInputDocWriter writer = new SystemInputDocWriter( outputStream))
      {
      writer.write( inputDef);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write input definition", e);
      }
    }

  /**
   * Writes an XML document describing the given test case definitions to the given output stream.
   */
  public static void writeTests( SystemTestDef testDef, OutputStream outputStream)
    {
    try( SystemTestDocWriter writer = new SystemTestDocWriter( outputStream))
      {
      writer.write( testDef);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write test definitions", e);
      }
    }

  /**
   * Writes an XML document describing the given generator definitions to the given output stream.
   */
  public static void writeGenerators( IGeneratorSet generators, OutputStream outputStream)
    {
    try( GeneratorSetDocWriter writer = new GeneratorSetDocWriter( outputStream))
      {
      writer.write( generators);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write generator definitions", e);
      }
    }

  /**
   * Returns test case definitions for the {@link SystemInputDef system input model}, 
   * {@link IGeneratorSet generator set} and {@link SystemTestDef base test definitions} defined by
   * the given {@link Project}. If no generator set is specified, the default generator is
   * used.  If no base test definitions are specified, new test cases are generated.
   */
  public static SystemTestDef getTests( Project project)
    {
    try
      {
      return
        Tcases.getTests
        ( project.getSystemInput(),
          project.getGenerators(),
          project.getBaseTests());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get test case definitions", e);
      }
    }
  }
