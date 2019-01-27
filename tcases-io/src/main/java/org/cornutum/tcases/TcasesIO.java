//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.generator.io.*;
import org.cornutum.tcases.io.*;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Generates a set of {@link TestCase test cases} from {@link SystemInputDef system input definition} documents.
 *
 */
public class TcasesIO
  {  
  /**
   * Creates a new Tcases object.
   */
  private TcasesIO()
    {
    // Static methods only
    }

  /**
   * Returns test case definitions for the given system input definition, using the given generator set and
   * base test definitions. If <CODE>genDef</CODE> is null, the default generator is used.
   * If <CODE>baseDef</CODE> is null, no base test definitions are used.
   */
  public static SystemTestDef getTests( InputStream inputDefStream, InputStream genDefStream, InputStream baseDefStream)
    {
    try
      {
      return
        Tcases.getTests
        ( new SystemInputDocReader( inputDefStream).getSystemInputDef(),
          genDefStream==null? null : new GeneratorSetDocReader( genDefStream).getGeneratorSet(),
          baseDefStream==null? null : new SystemTestDocReader( baseDefStream).getSystemTestDef());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get test definitions", e);
      }
    }

  /**
   * Returns new test case definitions for the given system input definition, using the default generator.
   */
  public static SystemTestDef getTests( InputStream inputDefStream)
    {
    return getTests( inputDefStream, null, null);
    }

  /**
   * Writes an XML document describing given test case definitions to the given output stream.
   */
  @SuppressWarnings("resource")
  public static void writeTests( SystemTestDef testDef, OutputStream outputStream)
    {
    try
      {
      SystemTestDocWriter writer = new SystemTestDocWriter( outputStream);
      writer.write( testDef);
      writer.flush();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write test definitions", e);
      }
    }
  }
