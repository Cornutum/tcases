//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.openapi.resolver.RequestCaseException;
import org.cornutum.tcases.openapi.testwriter.TestWriterTest;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileOutputStream;

/**
 * Base clase for tests that use a Moco server.
 */
public abstract class MocoServerTest extends TestWriterTest
  {
  /**
   * Writes the Moco server config file for the specified {@link RequestTestDef}.
   */
  protected void writeMocoServerConfig( String testDefName)
    {
    MocoServerConfigWriter writer = null;
    try
      {
      writer = new MocoServerConfigWriter( new FileOutputStream( mocoServerConfigFile( testDefName)));
      writer.write( stdRequestTestDef( testDefName));
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't write Moco server config for testDef=%s", testDefName), e);
      }
    finally
      {
      IOUtils.closeQuietly( writer, null);
      }
    }

  /**
   * Returns the target Moco server config file for the specified {@link RequestTestDef}.
   */
  protected File mocoServerConfigFile( String testDefName)
    {
    return new File( getResourceDir(), mocoServerConfigFor( testDefName));
    }

  /**
   * Returns the resource path to the Moco server configuration file for the specified standard {@link RequestTestDef} object.
   */
  protected String stdMocoServerConfig( String testDefName)
    {
    return
      String.format(
        "%s/%s",
        MocoServerTest.class.getPackage().getName().replaceAll( "\\.", "/"),
        mocoServerConfigFor( testDefName));
    }

  /**
   * Returns the name of the  Moco server configuration file for the specified standard {@link RequestTestDef} object.
   */
  protected String mocoServerConfigFor( String testDefName)
    {
    return String.format( "%s-Moco.json", testDefName);
    }

  }
