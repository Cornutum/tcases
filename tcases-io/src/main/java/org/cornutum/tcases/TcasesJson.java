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

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Generates a set of {@link TestCase test cases} from JSON documents that define the {@link SystemInputDef system input model},
 * {@link IGeneratorSet generators}, and {@link SystemTestDef base tests} for a Tcases {@link Project project}.
 */
public class TcasesJson
  {  
  /**
   * Creates a new TcasesJson object.
   */
  private TcasesJson()
    {
    // Static methods only
    }

  /**
   * Returns test case definitions for the {@link SystemInputDef system input model}, {@link IGeneratorSet generator set} and
   * {@link SystemTestDef base test definitions} defined by the {@link Project} read from the given {@link ProjectJsonReader JSON document stream}. 
   * If no generator set is specified, the default generator is used.  If no base test definitions are specified, new
   * test cases are generated.
   */
  public static SystemTestDef getTests( InputStream projectStream)
    {
    try( ProjectJsonReader reader = new ProjectJsonReader( projectStream))
      {
      return TcasesIO.getTests( reader.getProject());
      }
    }

  /**
   * Writes a {@link SystemInputJsonWriter JSON document} describing the given system input definition to the given output stream.
   */
  public static void writeInputModel( SystemInputDef inputDef, OutputStream outputStream)
    {
    try( SystemInputJsonWriter writer = new SystemInputJsonWriter( outputStream))
      {
      writer.write( inputDef);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write input definition", e);
      }
    }

  /**
   * Writes a {@link SystemTestJsonWriter JSON document} describing the given test case definitions to the given output stream.
   */
  public static void writeTests( SystemTestDef testDef, OutputStream outputStream)
    {
    try( SystemTestJsonWriter writer = new SystemTestJsonWriter( outputStream))
      {
      writer.write( testDef);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write test definitions", e);
      }
    }

  /**
   * Writes a JSON document describing the given generator definitions to the given output stream.
   */
  public static void writeGenerators( IGeneratorSet generators, OutputStream outputStream)
    {
    try( GeneratorSetJsonWriter writer = new GeneratorSetJsonWriter( outputStream))
      {
      writer.write( generators);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't write generator definitions", e);
      }
    }
  }
